{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types for NBP3
module KD8ZRC.Flight.NBP3.Types where

import Control.Lens
import qualified Data.ByteString.Char8 as BRC
import Data.Geo.Coordinate
import Data.List (dropWhileEnd)
import qualified Data.Text as T
import Data.Thyme.Clock
import KD8ZRC.Flight.NBP3.CRC
import KD8ZRC.Mapview.Types
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
  } deriving (Eq)
makeLenses ''TelemetryLine

instance HasCRC TelemetryLine Integer where
  telemetryCRC = TelemetryCRC . _crc
  calculatedCRC l =
    crcHaskell . dropWhileEnd (/= ':') . init . tail . BRC.unpack $ _rawLine l
