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

newtype HistorySaveCallback t = HistorySaveCallback (FilePath -> t -> IO ())

-- | To store the history of a flight, we write out a simple JSON file. This is
-- not part of the standard callback set for now, because different frontends
-- and projects have different requirements for what needs to be saved.
--
-- This saves only the coordinates, not other data in the packet. This is useful
-- for sending a history of where the balloon has been to frontends so they can
-- generate a visual path based on the numbers, without having to waste
-- bandwidth sending historical data that will never be used.
saveCoordinateHistory :: HistorySaveCallback TelemetryLine
saveCoordinateHistory = HistorySaveCallback $ \fp pkt -> do
  old <- S.readFile fp
  let history = decode (BSL.pack old) :: Maybe [JSONCoordinate]
      latestCoords = pkt ^. coordinates . to JSONCoordinate
  writeFile fp (BSL.unpack $ encode (latestCoords : fromMaybe [] history))

-- | However, there /are/ cases where we *do* care about the other data, in a
-- historical, machine-readable sense, such as when we want to generate graphs
-- after a flight.
saveFullHistory :: HistorySaveCallback TelemetryLine
saveFullHistory = HistorySaveCallback $ \fp pkt -> do
  old <- S.readFile fp
  let history = decode (BSL.pack old) :: Maybe [TelemetryLine]
  writeFile fp (BSL.unpack $ encode (pkt : fromMaybe [] history))

writePktHistory
  :: FilePath
  -> HistorySaveCallback t
  -> ParsedPacketCallback t
writePktHistory fp (HistorySaveCallback f) =
  ParseSuccessCallback (\pkt -> liftIO $ f fp pkt)
