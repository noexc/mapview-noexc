-- | Parser module for NBP3
module KD8ZRC.Flight.NBP3.Parser where

import Data.Char (digitToInt)
import Data.Geo.Coordinate
import Data.List (foldl')
import qualified Data.Text as T
import Data.Thyme.Format
import KD8ZRC.Flight.NBP3.Types
import Text.Trifecta
import System.Locale

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

    eitherToNum :: (Num b, Integral a) => Either a b -> b
    eitherToNum = either fromIntegral id
