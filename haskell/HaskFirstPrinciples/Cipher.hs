module Cipher where

import Data.Bool (bool)
import Data.Char

caesar :: Int -> String -> String
caesar shift = map (shiftAlpha (+) shift)

uncaesar :: Int -> String -> String
uncaesar shift = map (shiftAlpha (-) shift)

shiftAlpha :: (Int -> Int -> Int) -> Int -> Char -> Char
shiftAlpha op shift c
  | (&&) <$> isAscii <*> isAlpha $ c =
      chr . (+ baseChar) . (`mod` 26) . (`op` shift) . subtract baseChar . ord $ c
  | otherwise =
      '?'
  where baseChar = ord $ bool 'A' 'a' (isLower c)
