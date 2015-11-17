{-# LANGUAGE OverloadedStrings #-}
-- | This is the runner program for NBP-3. It ties together things from mapview
-- (the core library), helper functions from other KD8ZRC.Flight.NBP3.* modules,
-- and so on, into one program.
module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import KD8ZRC.Flight.NBP3.Parser
import KD8ZRC.Flight.NBP3.Types
import KD8ZRC.Mapview.Execute
import KD8ZRC.Mapview.Types
import KD8ZRC.Mapview.Utility.Downlink
import KD8ZRC.Mapview.Utility.Logging

-- | A simple example to show how a logger thread might work.
data Logger = Logger (MVar String)

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  _ <- forkIO (logVia m)
  return l

logVia :: MVar String -> IO ()
logVia v = do
  str <- takeMVar v
  putStrLn $ "Printed from another thread: " ++ str
  logVia v

logPacket :: Logger -> PacketLineCallback t
logPacket (Logger m) = PacketLineCallback (liftIO . putMVar m)

mvConfig :: Logger -> MapviewConfig TelemetryLine
mvConfig logger = MapviewConfig {
    _mvParser = parser
  , _mvDownlinkSpawn =
      modemStdout "minimodem" ["-r", "-q", "rtty", "-S", "700", "-M", "870"]
  , _mvPacketLineCallback =
      [ logRawPacketFile "/tmp/nbp3.log"
      , logRawPacketStdout
      , logPacket logger
      ]
  , _mvParsedPacketCallback = logParsedPacketStdout
}

main :: IO ()
main = do
  logger <- initLogger
  mapview (mvConfig logger)
