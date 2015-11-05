{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Parser, callback functions, and types for
-- <https://noexc.org/wiki/NBP3 NBP3>.
module KD8ZRC.Flight.NBP3 where

import Control.Lens
import qualified Data.ByteString as BR
import qualified Data.ByteString.Char8 as BRC
import Data.Char (digitToInt)
import Data.Digest.CRC16
import Data.Geo.Coordinate
import Data.List (dropWhileEnd, foldl')
import qualified Data.Text as T
import Data.Thyme.Clock
import Data.Thyme.Format
import Data.Word
import KD8ZRC.Mapview.Types
import KD8ZRC.Mapview.Utility.CRC
import Text.Trifecta
import System.Locale

type Meters = Double -- TODO

-- TODO: Find a better place for this stuff
crcHaskellF :: Word16 -> Bool -> Word16 -> [Word8] -> Word16
crcHaskellF poly inverse initial = BR.foldl (crc16Update poly inverse) initial . BR.pack

crcHaskell :: String -> CalculatedCRC Integer
crcHaskell s =
  CalculatedCRC . fromIntegral $ crcHaskellF
    0x1021
    False
    0xffff
    [fromIntegral (fromEnum x) :: Word8 | x <- s]

eitherToNum :: (Num b, Integral a) => Either a b -> b
eitherToNum = either fromIntegral id

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

-- | The parser for our telemetry packets. Uses the @parsers@ package and should
-- work with any supported parser combinator library.
--
-- See <https://noexc.org/wiki/NBP/RTTY_Telemetry_Format_v2> for more details.
parser :: (Monad m, DeltaParsing m) => m TelemetryLine
parser = do
  _ <- colon
  callsign' <- manyTill anyChar (try colon)
  lat' <- eitherToNum <$> integerOrDouble
  _ <- colon
  lon' <- eitherToNum <$> integerOrDouble
  _ <- colon
  altitude' <- eitherToNum <$> integerOrDouble
  _ <- colon
  time' <- many (token digit)
  _ <- colon
  crc16T <- number 16 hexDigit
  _ <- colon
  --crc16C <- crcHaskell . dropWhileEnd (/= ':') . init . tail . BRC.unpack <$> line
  raw <- line

  -- TODO: Fix partiality. Anything raised here will be caught by Trifecta,
  -- though, so it's not urgent. We won't ever hit _|_. It's just ugly.
  let Just coordinate = lat' <°> lon'
  return $ TelemetryLine
    raw
    (T.pack callsign')
    coordinate
    altitude'
    (readTime defaultTimeLocale "%H%M%S" time')
    crc16T
  where
    number base baseDigit =
      foldl' (\x d -> base * x + toInteger (digitToInt d)) 0 <$> some baseDigit
