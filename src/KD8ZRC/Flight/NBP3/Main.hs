{-# LANGUAGE OverloadedStrings #-}

-- | This is the runner program for NBP-3. It ties together things from mapview
-- (the core library), helper functions from other KD8ZRC.Flight.NBP3.* modules,
-- and so on, into one program.
module Main where

import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Monad.IO.Class
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Directory (createDirectoryIfMissing, doesFileExist)

import KD8ZRC.Flight.NBP3.History
import KD8ZRC.Flight.NBP3.Parser
import KD8ZRC.Flight.NBP3.Types
import KD8ZRC.Mapview.Execute
import KD8ZRC.Mapview.Types
import KD8ZRC.Mapview.Utility.Downlink
import KD8ZRC.Mapview.Utility.Logging
import KD8ZRC.Mapview.Utility.Websocket

mvConfig :: Chan.Chan BS.ByteString -> MapviewConfig TelemetryLine
mvConfig _ch = MapviewConfig {
    _mvParser = parser
  , _mvDownlinkSpawn =
      --modemStdout "minimodem" ["-r", "-q", "-S", "700", "-M", "1050", "-5", "110"]
      modemStdout "minimodem" ["-r", "rtty", "-q", "-S", "700", "-M", "870"]
  , _mvPacketLineCallback =
      [ logRawPacketFile "/var/tmp/nbp3/raw.log"
      , logRawPacketStdout
      ]
  , _mvParsedPacketCallback =
      [ writeChanPkt _ch
      , writePktHistory "/var/tmp/nbp3/history.json"
      ] ++ logParsedPacketStdout
}

writeChanPkt :: ToJSON t => Chan.Chan BS.ByteString -> ParsedPacketCallback t
writeChanPkt ch =
  ParseSuccessCallback (
    \pkt -> liftIO $ Chan.writeChan ch (BSL.toStrict . encode $ pkt))

sendWSHistory :: WebsocketOnConnectCallback
sendWSHistory =
  WebsocketOnConnectCallback (
    \(_, ch) -> do
      hist <- BS.readFile "/var/tmp/nbp3/history.json"
      Chan.writeChan ch hist)

createFileIfMissing :: FilePath -> IO ()
createFileIfMissing fp = do
  fileExists <- doesFileExist fp
  if fileExists then return () else writeFile fp ""

main :: IO ()
main = do
  createDirectoryIfMissing True "/var/tmp/nbp3"
  createFileIfMissing "/var/tmp/nbp3/history.json"
  rawChan <- Chan.newChan
  _ <- forkIO $ initWebsocketServer rawChan "0.0.0.0" 9160 [sendWSHistory]
  mapview (mvConfig rawChan)
