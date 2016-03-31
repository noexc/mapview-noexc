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
  createHistoryFileHierarchy
  rawChan <- Chan.newChan
  _ <- forkIO $ initWebsocketServer rawChan "0.0.0.0" 9160 [sendWSHistory cHist]
  putStrLn "Welcome to Mapview for NBP4!"
  mapview (mvConfig rawChan)
