{-# LANGUAGE OverloadedStrings #-}
-- | This is the runner program for NBP-3. It ties together things from mapview
-- (the core library), helper functions from other KD8ZRC.Flight.NBP3.* modules,
-- and so on, into one program.
module Main where

import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Monad.IO.Class
import Data.Aeson (ToJSON, encode)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import KD8ZRC.Flight.NBP3.Parser
import KD8ZRC.Flight.NBP3.Types
import KD8ZRC.Mapview.Execute
import KD8ZRC.Mapview.Types
--import KD8ZRC.Mapview.Utility.Concurrent
import KD8ZRC.Mapview.Utility.Downlink
import KD8ZRC.Mapview.Utility.Logging
import KD8ZRC.Mapview.Utility.Websocket

mvConfig :: Chan.Chan T.Text -> MapviewConfig TelemetryLine
mvConfig _ch = MapviewConfig {
    _mvParser = parser
  , _mvDownlinkSpawn =
      modemStdout "minimodem" ["-r", "-q", "rtty", "-S", "700", "-M", "870"]
  , _mvPacketLineCallback =
      [ logRawPacketFile "/tmp/nbp3.log"
      , logRawPacketStdout
      --, writeChanRaw _ch
      ]
  , _mvParsedPacketCallback =
      [ writeChanPkt _ch
      ] ++ logParsedPacketStdout
}

writeChanPkt :: ToJSON t => Chan.Chan T.Text -> ParsedPacketCallback t
writeChanPkt ch =
  ParseSuccessCallback (
    \pkt -> liftIO $ Chan.writeChan ch (TL.toStrict . TL.decodeUtf8 . encode $ pkt))

main :: IO ()
main = do
  rawChan <- Chan.newChan
  _ <- forkIO $ initWebsocketServer rawChan "127.0.0.1" 8181
  mapview (mvConfig rawChan)
