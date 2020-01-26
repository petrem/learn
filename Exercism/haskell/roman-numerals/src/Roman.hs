module Roman (numerals) where

import Data.Char (digitToInt)

--numerals :: Integer -> Maybe String
numerals n = Just . concat $ zipWith (\a rs -> rs !! digitToInt a) (reverse . show $ n) romanDigits


romanDigits :: [[String]]
romanDigits = [["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
              ,["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"]
              ,["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"]
              ,["", "M", "MM", "MMM", "MMMM", "MMMMM", "MMMMMM", "MMMMMMM", "MMMMMMMM", "MMMMMMMMM"]
              ]
