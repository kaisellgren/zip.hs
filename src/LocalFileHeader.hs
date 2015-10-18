module LocalFileHeader where

import Prelude hiding ((++), map, length)
import qualified Prelude as P
import Data.Char
import Data.Word
import Data.Vector.Unboxed
import Signature
import Util

-- Local file header:
--
-- local file header signature     4 bytes  (0x04034b50)
-- version needed to extract       2 bytes
-- general purpose bit flag        2 bytes
-- compression method              2 bytes
-- last mod file time              2 bytes
-- last mod file date              2 bytes
-- crc-32                          4 bytes
-- compressed size                 4 bytes
-- uncompressed size               4 bytes
-- file name length                2 bytes
-- extra field length              2 bytes
--
-- file name (variable size)
-- extra field (variable size)

data LocalFileHeader = LocalFileHeader {
    versionNeededToExtract :: Word16,
    generalPurposeBitFlag :: Word16,
    compressionMethod :: Word16,
    lastModifiedFileTime :: Word16,
    lastModifiedFileDate :: Word16,
    crc32 :: Word32,
    compressedSize :: Word32,
    uncompressedSize :: Word32,
    filenameLength :: Word16,
    extraFieldLength :: Word16,
    filename :: String,
    extraField :: Vector Word8,
    content :: Vector Word8
}

save :: LocalFileHeader -> Vector Word8
save record =
    empty ++
    localFileHeader ++
    (numToBytes $ versionNeededToExtract record) ++
    (numToBytes $ generalPurposeBitFlag record) ++
    (numToBytes $ compressionMethod record) ++
    (numToBytes $ lastModifiedFileTime record) ++
    (numToBytes $ lastModifiedFileDate record) ++
    (numToBytes $ crc32 record) ++
    (numToBytes $ compressedSize record) ++
    (numToBytes $ uncompressedSize record) ++
    (intToBytes (P.length $ filename record) 2) ++
    (intToBytes (length $ extraField record) 2) ++
    (stringToBytes $ filename record) ++
    (extraField record) ++
    (content record)

load :: Vector Word8 -> LocalFileHeader
load bytes = LocalFileHeader {
        versionNeededToExtract = bytesToNum $ slice 4 2 bytes,
        generalPurposeBitFlag = bytesToNum $ slice 6 2 bytes,
        compressionMethod = bytesToNum $ slice 8 2 bytes,
        lastModifiedFileTime = bytesToNum $ slice 10 2 bytes,
        lastModifiedFileDate = bytesToNum $ slice 12 2 bytes,
        crc32 = bytesToNum $ slice 14 4 bytes,
        compressedSize = compressedSize,
        uncompressedSize = bytesToNum $ slice 22 4 bytes,
        filenameLength = filenameLength,
        extraFieldLength = extraFieldLength,
        filename = toList $ map (chr . fromIntegral) $ slice 30 filenameLengthFI bytes,
        extraField = slice (30 + filenameLengthFI) extraFieldLengthFI bytes,
        content = slice (30 + filenameLengthFI + extraFieldLengthFI) (fromIntegral compressedSize) bytes
    }
      where filenameLength = bytesToNum $ slice 26 2 bytes
            extraFieldLength = bytesToNum $ slice 28 2 bytes
            compressedSize = bytesToNum $ slice 18 4 bytes
            filenameLengthFI = (fromIntegral filenameLength)
            extraFieldLengthFI = (fromIntegral extraFieldLength)
