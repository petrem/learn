-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

import qualified Data.List as List


encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . List.group

encode' :: (Eq a) => [a] -> [(Int, a)]
encode' = foldr encoder []
  where encoder x [] = [(1,x)]
        encoder x (xs@(n,v):ys)
          | x == v = (n+1,v):ys
          | otherwise = (1,x):xs:ys
