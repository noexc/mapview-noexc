-- | Parser module for NBP3
module KD8ZRC.Flight.NBP3.Parser where

import qualified Data.ByteString.Char8 as BRC
import Data.Char (digitToInt)
import Data.Geo.Coordinate
import Data.List (dropWhileEnd, foldl')
import qualified Data.Text as T
import Data.Thyme.Format
import KD8ZRC.Flight.NBP3.CRC
import KD8ZRC.Flight.NBP3.Types
import KD8ZRC.Mapview.Utility.CRC
import Text.Trifecta
import System.Locale

-- | The parser for our telemetry packets. Uses the @parsers@ package and should
-- work with any supported parser combinator library.
--
-- See <https://noexc.org/wiki/NBP/RTTY_Telemetry_Format_v2> for more details.
parser :: (Monad m, DeltaParsing m, Errable m) => m TelemetryLine
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
  crc16C <- crcHaskell . dropWhileEnd (/= ':') . init . tail . BRC.unpack <$> line
  raw <- line

  case lat' <°> lon' of
    Nothing ->
      raiseErr $ failed "Unable to produce a Coordinate from the given lat/lon pair."
    Just coordinate -> do
      let crcConfirmation = validateCRC (TelemetryCRC crc16T) crc16C
      case crcConfirmation of
        CRCMismatch (TelemetryCRC t) (CalculatedCRC c) ->
          raiseErr $ failed ("CRC Mismatch: Downlink=" ++
                             show t ++ " Expected=" ++ show c)
        _ -> return $ TelemetryLine
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
