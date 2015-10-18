module EndOfCentralDirectoryRecord where

import Prelude hiding ((++), map)
import Data.Char
import Data.Word
import Data.Vector.Unboxed
import Signature
import Util

-- I.  End of central directory record:
--
--   end of central dir signature    4 bytes  (0x06054b50)
--   number of this disk             2 bytes
--   number of the disk with the
--   start of the central directory  2 bytes
--   total number of entries in the
--   central directory on this disk  2 bytes
--   total number of entries in
--   the central directory           2 bytes
--   size of the central directory   4 bytes
--   offset of start of central
--   directory with respect to
--   the starting disk number        4 bytes
--   .ZIP file comment length        2 bytes
--   .ZIP file comment               (variable size)

data EndOfCentralDirectoryRecord = EndOfCentralDirectoryRecord {
    diskNumber :: Word16,
    diskNumberWithTheCentralDirectory :: Word16,
    totalCentralDirectoryEntriesOnThisDisk :: Word16,
    totalCentralDirectoryEntries :: Word16,
    centralDirectorySize :: Word32,
    centralDirectoryOffset :: Word32,
    zipFileComment :: String
}

save :: EndOfCentralDirectoryRecord -> Vector Word8
save record =
    empty ++
    endOfCentralDirectoryRecord ++
    (numToBytes $ diskNumber record) ++
    (numToBytes $ diskNumberWithTheCentralDirectory record) ++
    (numToBytes $ totalCentralDirectoryEntriesOnThisDisk record) ++
    (numToBytes $ totalCentralDirectoryEntries record) ++
    (numToBytes $ centralDirectorySize record) ++
    (numToBytes $ centralDirectoryOffset record) ++
    (intToBytes (Prelude.length $ zipFileComment record) 2) ++
    (stringToBytes $ zipFileComment record)

load :: Vector Word8 -> EndOfCentralDirectoryRecord
load bytes = EndOfCentralDirectoryRecord {
        diskNumber = bytesToNum $ slice 4 2 bytes,
        diskNumberWithTheCentralDirectory = bytesToNum $ slice 6 2 bytes,
        totalCentralDirectoryEntriesOnThisDisk = bytesToNum $ slice 8 2 bytes,
        totalCentralDirectoryEntries = bytesToNum $ slice 10 2 bytes,
        centralDirectorySize = bytesToNum $ slice 12 4 bytes,
        centralDirectoryOffset = bytesToNum $ slice 16 4 bytes,
        zipFileComment = toList $ map (chr . fromIntegral) (slice 22 zipFileCommentLength bytes)
    }
      where zipFileCommentLength = bytesToInt $ slice 20 2 bytes
