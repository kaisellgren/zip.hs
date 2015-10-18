module Main where

import System.Exit
import EndOfCentralDirectoryRecord
import LocalFileHeader
import Util
import qualified Data.Vector.Unboxed as Vec

main = do
    print $ intToBytes (256*256+2) 4
    print $ bytesToInt $ Vec.fromList [2,0,1,0]
    exitFailure
