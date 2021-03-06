{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types for NBP4
module KD8ZRC.Flight.NBP4.Types where

import Control.Lens hiding ((.=))
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Char8 as BRC
import Data.Geo.Coordinate
import qualified Data.Text as T
import Data.Thyme.Clock
import Data.Thyme.Format
import Data.Thyme.Format.Aeson ()
import System.Locale (defaultTimeLocale)

type Meters = Double -- TODO

data TelemetryLine = TelemetryLine {
    _rawLine     :: BRC.ByteString
    -- ^ We keep the raw line around for any callbacks that need it, but also
    -- so that we can define a 'HasCRC' instance that makes use of it.
  , _callsign    :: T.Text
  , _coordinates :: Coordinate
  , _altitude    :: Meters
  , _time        :: UTCTime
  , _voltage     :: Double
  , _crc         :: Integer
  } deriving (Eq, Show)
makeLenses ''TelemetryLine

newtype JSONCoordinate = JSONCoordinate Coordinate

instance ToJSON JSONCoordinate where
    toJSON (JSONCoordinate coords) =
        object [ "lat" .= fst coordinatesTuple
               , "lon" .= snd coordinatesTuple
               ]
      where
        coordinatesTuple =
          _Coordinate  # (coords ^. _Coordinate) :: (Double, Double)

instance FromJSON JSONCoordinate where
    parseJSON (Object v) = do
      lat <- v .: "lat"
      lon <- v .: "lon"
      case lat <°> lon of
        Nothing -> fail "Failed to parse lat/lon pair from history file"
        Just coordinate -> return $ JSONCoordinate coordinate
    parseJSON _          = mzero

instance ToJSON TelemetryLine where
  -- We don't include _rawLine here for a few reasons.
  -- One, it's a ByteString, which could in theory contain binary data.
  -- Two, it isn't necessary for anything I wish to show in our web UI.
  -- Three, there is a callback we offer in Utility.Concurrent for sending
  -- raw lines to a Chan. This could be used to send them to the websocket
  -- if necessary.
  -- Of course, if you are someone who is looking at this code as just an
  -- example, you could, in theory, do something like use 'decodeUtf8' from
  -- 'Data.Text.Encoding' if you were sure the line never contained binary data.
    toJSON TelemetryLine{..} =
        object [ "callsign"    .= _callsign
               , "coordinates" .= JSONCoordinate _coordinates
               , "altitude"    .= _altitude
               , "time"        .= formatTimestamp _time
               , "voltage"     .= _voltage
               , "crc"         .= _crc
               ]
      where
        formatTimestamp = formatTime defaultTimeLocale "%T UTC"

-- | This instance is used for saving machine-readable history of packets.
--
-- See KD8ZRC.Flight.NBP4.History for more information.
--
-- NOTE: The JSON encoding of a 'TelemetryLine' is **NOT* isomorphic to
-- 'TelemetryLine' itself. Notably, we cannot recover a full 'TelemetryLine'
-- because we drop the '_rawLine' when we serialize above. Here we accommodate
-- for that by using 'mempty' for 'BRC.ByteString'.
instance FromJSON TelemetryLine where
    parseJSON (Object v) = do
      callsign' <- v .: "callsign"
      JSONCoordinate coords' <- v .: "coordinates"
      altitude' <- v .: "altitude"
      time' <- v .: "time"
      voltage' <- v .: "voltage"
      crc' <- v .: "crc"
      return $
        TelemetryLine mempty callsign' coords' altitude' time' voltage' crc'
    parseJSON _          = mzero
