{-# LANGUAGE OverloadedStrings #-}
-- | This is the runner program for NBP-3. It ties together things from mapview
-- (the core library), helper functions from other KD8ZRC.Flight.NBP3.* modules,
-- and so on, into one program.
module Main where

import KD8ZRC.Flight.NBP3.Parser
import KD8ZRC.Flight.NBP3.Types
import KD8ZRC.Mapview.Execute
import KD8ZRC.Mapview.Types
import KD8ZRC.Mapview.Utility.Downlink
import KD8ZRC.Mapview.Utility.Logging
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import System.IO

mvConfig :: MapviewConfig TelemetryLine
mvConfig = MapviewConfig {
    _mvParser = parser
  , _mvDownlinkSpawn =
      modemStdout "minimodem" ["-r", "-q", "rtty", "-S", "700", "-M", "870"]
  , _mvPacketLineCallback =
      [ logRawPacketFile "/tmp/nbp3.log"
      , logRawPacketStdout
      ]
  , _mvParsedPacketCallback = logParsedPacketStdout
  , _mvForkProcesses = [test]
}

test :: MapviewProcess t
test = MapviewProcess $ do
  liftIO $ do
    hSetBuffering stdout NoBuffering
    forever $ do
      putStrLn "ping!"
      threadDelay 10000000

main :: IO ()
main = mapview mvConfig
