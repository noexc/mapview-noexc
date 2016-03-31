{-# LANGUAGE OverloadedStrings #-}

-- | This is the runner program for NBP4. It ties together things from mapview
-- (the core library), helper functions from other KD8ZRC.Flight.NBP4.* modules,
-- and so on, into one program.
module Main where

import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Monad.IO.Class
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)

import KD8ZRC.Flight.NBP4.History
import KD8ZRC.Flight.NBP4.Parser
import KD8ZRC.Flight.NBP4.Types
import KD8ZRC.Mapview.Execute
import KD8ZRC.Mapview.Types
import KD8ZRC.Mapview.Utility.Downlink
import KD8ZRC.Mapview.Utility.Logging
import KD8ZRC.Mapview.Utility.Websocket

flightId :: String
flightId = "nbp4"

cHist, fHist, rawLog :: FilePath
cHist = "/var/tmp/" ++ flightId ++ "/history-coord.json"
fHist = "/var/tmp/" ++ flightId ++ "/history-full.json"
rawLog = "/var/tmp/" ++ flightId ++ "/raw.log"

mvConfig :: Chan.Chan BS.ByteString -> MapviewConfig TelemetryLine
mvConfig _ch = MapviewConfig {
    _mvParser = parser
  , _mvDownlinkSpawn =
      --modemStdout "minimodem" ["-r", "-q", "-S", "700", "-M", "1050", "-5", "110"]
      modemStdout "minimodem" ["-r", "rtty", "-q", "-S", "700", "-M", "870"]
  , _mvPacketLineCallback =
      [ logRawPacketFile rawLog
      , logRawPacketStdout
      ]
  , _mvParsedPacketCallback =
      [ writeChanPkt _ch
      , writePktHistory cHist saveCoordinateHistory
      , writePktHistory fHist saveFullHistory
      ] ++ logParsedPacketStdout
  }

-- | Strip all callbacks except those necessary for a PRE-LAUNCH test.
mvConfigTestMode
  :: Chan.Chan BS.ByteString
  -> MapviewConfig TelemetryLine
  -> MapviewConfig TelemetryLine
mvConfigTestMode _ch c = c { _mvPacketLineCallback = [logRawPacketStdout]
                       , _mvParsedPacketCallback =
                           [writeChanPkt _ch] ++ logParsedPacketStdout
                       }

writeChanPkt :: ToJSON t => Chan.Chan BS.ByteString -> ParsedPacketCallback t
writeChanPkt ch =
  ParseSuccessCallback (
    \pkt -> liftIO $ Chan.writeChan ch (BSL.toStrict . encode $ pkt))

-- TODO: This should move somewhere.
createFileIfMissing :: FilePath -> IO ()
createFileIfMissing fp = do
  fileExists <- doesFileExist fp
  if fileExists then return () else writeFile fp ""

createHistoryFileHierarchy :: IO ()
createHistoryFileHierarchy = do
  createDirectoryIfMissing True ("/var/tmp/" ++ flightId)
  createFileIfMissing cHist
  createFileIfMissing fHist

main :: IO ()
main = do
  putStrLn "Welcome to Mapview for NBP4!"
  args <- getArgs
  if length args > 0 && head args == "TEST_MODE"
    then mainTest
    else mainProd

mainProd :: IO ()
mainProd = do
  createHistoryFileHierarchy
  rawChan <- Chan.newChan
  _ <- forkIO $ initWebsocketServer rawChan "0.0.0.0" 9160 [sendWSHistory cHist]
  mapview (mvConfig rawChan)

mainTest :: IO ()
mainTest = do
  putStrLn "!!! WARNING WARNING WARNING !!!"
  putStrLn "Mapview is currently running in TEST MODE"
  putStrLn "No data is being persisted!"
  putStrLn "!!! WARNING WARNING WARNING !!!"
  putStrLn ""
  rawChan <- Chan.newChan
  _ <- forkIO $ initWebsocketServer rawChan "0.0.0.0" 9160 [sendWSHistory cHist]
  mapview (mvConfigTestMode rawChan $ mvConfig rawChan)
