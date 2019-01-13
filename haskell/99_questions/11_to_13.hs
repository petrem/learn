--11. Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

import qualified Data.List as List

data Encoded a = Single a | Multiple a Int deriving Show
instance Functor Encoded where
  fmap f (Single e) = Single (f e)
  fmap f (Multiple e n) = Multiple (f e) n

encode :: (Eq a) => [a] -> [Encoded a]
encode = map _encodeGroup . List.group
  where _encodeGroup xs
          | length xs == 1 = Single (head xs)
          | otherwise = Multiple (head xs) (length xs)

encode' :: (Eq a) => [a] -> [Encoded a]
encode' = foldr encoder []
  where encoder x [] = [Single x]
        encoder x (y@(Single val):ys)
          | x == val = (Multiple val 2):ys
          | otherwise = (Single x):y:ys
        encoder x (y@(Multiple val n):ys)
          | x == val = (Multiple val (n+1)):ys
          | otherwise = (Single x):y:ys


-- 12. Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

decode :: [Encoded a] -> [a]
decode = foldr _decodeEncoded []
  where _decodeEncoded (Single e) xs = e:xs
        _decodeEncoded (Multiple e n) xs = List.replicate n e ++ xs

-- neat version copied from solutions
decode1 :: [(Int, a)] -> [a]
decode1 = concatMap (uncurry replicate)


-- 13. Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X. 

-- see 10.hs
