-- Eliminate consecutive duplicates of list elements.
import qualified Data.List as List
import qualified Data.Set as Set

compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' [x] = [x]
compress' (x:xs@(y:_)) = if x == y then (compress' xs) else x:(compress' xs)

compress'' :: (Eq a) => [a] -> [a]
compress'' = foldr (\x acc -> if acc /= [] && x == (head acc) then acc else x:acc) []

-- WRONG, does not preserve order and alterated groups like 1,1,1,2,1 -> 1,2
-- this does the same as List.nub:
compress''' :: (Ord a) => [a] -> [a]
compress''' = Set.toList . Set.fromList

compress4 :: (Eq a) => [a] -> [a]
compress4 = map head . List.group


-- Copied from solutions:

compress5 []     = []
compress5 (x:xs) = x : (compress5 $ dropWhile (== x) xs)
