module Acton.Fingerprint
  ( fingerprintPlaceholder
  , fingerprintPrefixForName
  , formatFingerprint
  , formatFingerprintPrefix
  , parseFingerprint
  , updateFingerprintPrefix
  ) where

import Data.Bits (shiftL, xor, shiftR, complement, (.|.), (.&.))
import Data.Char (isDigit, isHexDigit, isSpace, toLower)
import Data.Word (Word32, Word64)
import Numeric (readHex, readDec, showHex)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS

fingerprintPlaceholder :: String
fingerprintPlaceholder = "0xacedf00dacedf00d"

crc32IsoHdlc :: BS.ByteString -> Word32
crc32IsoHdlc bs = complement (BS.foldl' update 0xffffffff bs)
  where
    update crc byte = go 0 (crc `xor` fromIntegral byte)
    go 8 crc = crc
    go n crc =
        let crc' = if (crc .&. 1) /= 0
                     then (crc `shiftR` 1) `xor` 0xEDB88320
                     else crc `shiftR` 1
        in go (n + 1) crc'

fingerprintPrefixForName :: String -> Word32
fingerprintPrefixForName name =
    crc32IsoHdlc (B.pack name)

formatFingerprint :: Word64 -> String
formatFingerprint fp =
    "0x" ++ padHex 16 fp

formatFingerprintPrefix :: Word32 -> String
formatFingerprintPrefix fp =
    "0x" ++ padHex 8 (fromIntegral fp :: Word64)

updateFingerprintPrefix :: Word32 -> Word64 -> Word64
updateFingerprintPrefix prefix fp =
    (fromIntegral prefix `shiftL` 32) .|. (fp .&. 0xffffffff)

parseFingerprint :: String -> Maybe Word64
parseFingerprint raw =
    let trimmed = trim raw
    in case trimmed of
         '0':'x':rest -> parseHex rest
         '0':'X':rest -> parseHex rest
         _ | all isDigit trimmed -> parseDec trimmed
         _ -> Nothing
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

    parseHex s =
      case readHex (map toLower s) of
        [(n, "")] | n <= toInteger (maxBound :: Word64) -> Just (fromInteger n)
        _ -> Nothing

    parseDec s =
      case readDec s of
        [(n, "")] | n <= toInteger (maxBound :: Word64) -> Just (fromInteger n)
        _ -> Nothing

padHex :: Int -> Word64 -> String
padHex width value =
    let hex = showHex value ""
        zeros = replicate (max 0 (width - length hex)) '0'
    in zeros ++ hex
