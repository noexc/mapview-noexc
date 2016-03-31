{-# LANGUAGE CPP #-}
-- | Parser module for NBP4
module KD8ZRC.Flight.NBP4.Parser where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import qualified Data.ByteString.Char8 as BRC
import Data.Char (digitToInt)
import Data.Geo.Coordinate
import Data.List (dropWhileEnd, foldl')
import qualified Data.Text as T
import Data.Thyme.Format
import KD8ZRC.Flight.NBP4.CRC
import KD8ZRC.Flight.NBP4.Types
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
  voltage' <- eitherToNum <$> integerOrDouble
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
             (rawVoltageToRealVoltage voltage')
             crc16T
  where
    number base baseDigit =
      foldl' (\x d -> base * x + toInteger (digitToInt d)) 0 <$> some baseDigit

    eitherToNum :: (Num b, Integral a) => Either a b -> b
    eitherToNum = either fromIntegral id

    rawVoltageToRealVoltage x = roundToN 3 ((x + 477) / 339)

    roundToN :: Integer -> Double -> Double
    roundToN n f =  (fromInteger $ round $ f * (10^n)) / (10.0^^n)
