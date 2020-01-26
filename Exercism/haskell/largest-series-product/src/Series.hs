module Series (Error(..), largestProduct) where

import Data.Char (digitToInt)
import Data.List (tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits =  findLargestProduct =<< traverse eitherDigitToInteger digits
  where findLargestProducts = map product . filter ((==size) . length) . map (take size) . tails
        findLargestProduct [] = Left InvalidSpan
        findLargestProduct xs = Right . maximum . findLargestProducts $ xs

eitherDigitToInteger :: Char -> Either Error Integer
eitherDigitToInteger x | x `elem` ['0'..'9'] = Right . toInteger . digitToInt $ x
                       | otherwise = Left . InvalidDigit $ x

