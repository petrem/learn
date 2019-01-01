-- Find the K'th element of a list. The first element in the list is number 1.

elementAt :: Int -> [a] -> a
elementAt k xs = xs !! (k-1)

elementAt1 :: Int -> [a] -> a
elementAt1 0 _ = error "Zero?!" -- yeah there are negatives too
elementAt1 k (x:xs) = if k == 1 then x else elementAt1 (k-1) xs

elementAt2 :: Int -> [a] -> a
elementAt2 k xs = fst $ head $ filter snd $ zipWith (\x y -> (x, y == k)) xs [1..]


-- the following, copied from solutions, I do not understand:

elementAt''' xs n = head $ foldr ($) xs
                         $ replicate (n - 1) tail
