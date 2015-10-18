module Signature where

import Data.Word
import Data.Char
import Data.Vector.Unboxed

localFileHeader = fromList [0x50, 0x4b, 0x03, 0x04] :: Vector Word8
dataDescriptor = fromList [0x50, 0x4b, 0x07, 0x08] :: Vector Word8
centralDirectoryFileHeader = fromList [0x50, 0x4b, 0x01, 0x02] :: Vector Word8
endOfCentralDirectoryRecord = fromList [0x50, 0x4b, 0x05, 0x06] :: Vector Word8
centralDirectoryDigitalSignature = fromList [0x50, 0x4b, 0x05, 0x05] :: Vector Word8
