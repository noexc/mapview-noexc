-- | Helper CRC functions for NBP3. We don't actually define the 'HasCRC'
-- instance here, to avoid orphans.
module KD8ZRC.Flight.NBP3.CRC where

import qualified Data.ByteString as BR
import Data.Digest.CRC16
import Data.Word
import KD8ZRC.Mapview.Utility.CRC

crcHaskellF :: Word16 -> Bool -> Word16 -> [Word8] -> Word16
crcHaskellF poly inverse initial = BR.foldl (crc16Update poly inverse) initial . BR.pack

crcHaskell :: String -> CalculatedCRC Integer
crcHaskell s =
  CalculatedCRC . fromIntegral $ crcHaskellF
    0x1021
    False
    0xffff
    [fromIntegral (fromEnum x) :: Word8 | x <- s]
