{-# LANGUAGE OverloadedStrings #-}

-- | Functions to deal with coordinate history tracking.
module KD8ZRC.Flight.NBP4.History where

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)
import KD8ZRC.Flight.NBP4.Types
import KD8ZRC.Mapview.Types
import qualified System.IO.Strict as S

-- | To store the history of a flight, we write out a simple JSON file. This is
-- not part of the standard callback set for now, because different frontends
-- and projects have different requirements for what needs to be saved.
saveHistory
  :: FilePath -- ^ The file to save history to.
  -> TelemetryLine -- ^ The downlink packet containing the latest data
  -> IO ()
saveHistory fp pkt = do
  old <- S.readFile fp
  let history = decode (BSL.pack old) :: Maybe [JSONCoordinate]
      latestCoords = pkt ^. coordinates . to JSONCoordinate
  writeFile fp (BSL.unpack $ encode (latestCoords : fromMaybe [] history))

writePktHistory :: FilePath -> ParsedPacketCallback TelemetryLine
writePktHistory fp = ParseSuccessCallback (\pkt -> liftIO $ saveHistory fp pkt)
