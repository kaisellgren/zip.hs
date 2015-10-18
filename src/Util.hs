module Util where

import Prelude hiding ((++), length, tail, head)
import Data.Char
import Data.Bits
import Data.Word
import Data.Vector.Unboxed

class NumToBytes a where
    numToBytes :: a -> Vector Word8

instance NumToBytes Word8 where
    numToBytes a = intToBytes (fromIntegral a) 1

instance NumToBytes Word16 where
    numToBytes a = intToBytes (fromIntegral a) 2

instance NumToBytes Word32 where
    numToBytes a = intToBytes (fromIntegral a) 4

instance NumToBytes Word64 where
    numToBytes a = intToBytes (fromIntegral a) 8

intToBytes :: Int -> Int -> Vector Word8
intToBytes value minByteCount = intToBytes_ empty value minByteCount where
    intToBytes_ bytes value minByteCount
        | minByteCount == 0 = bytes
        | otherwise = intToBytes_ (snoc bytes byte) newValue (minByteCount - 1)
          where newValue = shift value (-8)
                byte = fromIntegral (value .&. 0xff) :: Word8

class BytesToNum a where
    bytesToNum :: Vector Word8 -> a

instance BytesToNum Word8 where
    bytesToNum a = fromIntegral $ bytesToInt a

instance BytesToNum Word16 where
    bytesToNum a = fromIntegral $ bytesToInt a

instance BytesToNum Word32 where
    bytesToNum a = fromIntegral $ bytesToInt a

bytesToInt :: Vector Word8 -> Int
bytesToInt bytes = bytesToInt_ bytes 0 0 where
    bytesToInt_ bytes value index
        | length bytes == 0 = value
        | otherwise = bytesToInt_ (tail bytes) (value + newValue) (index + 1)
          where newValue = fromIntegral $ (head bytes) * (256 ^ index)

stringToBytes :: String -> Vector Word8
stringToBytes = fromList . (fmap (fromIntegral . ord))
