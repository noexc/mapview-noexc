{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types for NBP3
module KD8ZRC.Flight.NBP3.Types where

import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.ByteString.Char8 as BRC
import Data.Geo.Coordinate
import Data.List (dropWhileEnd)
import qualified Data.Text as T
import Data.Thyme.Clock
import Data.Thyme.Format.Aeson ()
import KD8ZRC.Flight.NBP3.CRC
import KD8ZRC.Mapview.Utility.CRC

type Meters = Double -- TODO

-- | NOTE: Do not rely on this yet. It is going to change a lot. Seriously.
data TelemetryLine = TelemetryLine {
    _rawLine     :: BRC.ByteString
    -- ^ We keep the raw line around for any callbacks that need it, but also
    -- so that we can define a 'HasCRC' instance that makes use of it.
  , _callsign    :: T.Text
  , _coordinates :: Coordinate
  , _altitude    :: Meters
  , _time        :: UTCTime
  , _crc         :: Integer
  } deriving (Eq, Show)
makeLenses ''TelemetryLine

instance HasCRC TelemetryLine Integer where
  telemetryCRC = TelemetryCRC . _crc
  calculatedCRC l =
    crcHaskell . dropWhileEnd (/= ':') . init . tail . BRC.unpack $ _rawLine l

instance ToJSON TelemetryLine where
  -- We don't include -rawLine here for a few reasons.
  -- One, it's a ByteString, which could in theory contain binary data.
  -- Two, it isn't necessary for anything I wish to show in our web UI.
  -- Three, there is a callback we offer in Utility.Concurrent for sending
  -- raw lines to a Chan. This could be used to send them to the websocket
  -- if necessary.
  -- Of course, if you are someone who is looking at this code as just an
  -- example, you could, in theory, do something like use 'decodeUtf8' from
  -- 'Data.Text.Encoding' if you were sure the line never contained binary data.
    toJSON TelemetryLine{..} =
        object [ "coordinates" .= coordinatesTuple
               , "altitude"    .= _altitude
               , "time"        .= _time
               , "crc"         .= _crc
               ]
      where
        coordinatesTuple =
          _Coordinate  # (_coordinates ^. _Coordinate) :: (Double, Double)
