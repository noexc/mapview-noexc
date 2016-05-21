{-# LANGUAGE OverloadedStrings #-}

-- | This is the runner program for NBP4. It ties together things from mapview
-- (the core library), helper functions from other KD8ZRC.Flight.NBP4.* modules,
-- and so on, into one program.
module Main where

import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import qualified Data.ByteString.Char8 as BS
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)

import KD8ZRC.Flight.NBP4.History
import KD8ZRC.Flight.NBP4.Parser
import KD8ZRC.Flight.NBP4.Types
import KD8ZRC.Mapview.Execute
import KD8ZRC.Mapview.Types
import KD8ZRC.Mapview.Utility.Concurrent
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
      [ writeChanJsonPkt _ch
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
                                 [writeChanJsonPkt _ch] ++ logParsedPacketStdout
                           }

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
  rawChan <- Chan.newChan
  _ <- forkIO $ initWebsocketServer rawChan "0.0.0.0" 9160 [sendWSHistory cHist]
  if length args > 0 && head args == "TEST_MODE"
    then mainTest rawChan
    else mainProd rawChan

mainProd :: Chan.Chan BS.ByteString -> IO ()
mainProd rawChan = do
  createHistoryFileHierarchy
  mapview (mvConfig rawChan)

mainTest :: Chan.Chan BS.ByteString -> IO ()
mainTest rawChan = do
  putStrLn "!!! WARNING WARNING WARNING !!!"
  putStrLn "Mapview is currently running in TEST MODE"
  putStrLn "No data is being persisted!"
  putStrLn "!!! WARNING WARNING WARNING !!!"
  putStrLn ""
  mapview (mvConfigTestMode <*> mvConfig $ rawChan)
