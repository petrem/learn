module MakeOdd where

import Data.Bits (countTrailingZeros, shiftR)

makeOdd :: Int -> (Int, Int)
makeOdd x = let ctz = countTrailingZeros x in (ctz, shiftR x ctz)

