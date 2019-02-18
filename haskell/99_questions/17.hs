-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs 0 = ([], xs)
split (x:xs) n = (x:(fst $ split xs (n-1)), snd $ split xs (n-1))


split' :: [a] -> Int -> ([a], [a])
split' xs n = (\(i, l, r) -> (l, r)) $ foldl helper (n, [], []) xs
  where helper (i, l, r) x
          | i > 0     = (i-1, l++[x], r)
          | otherwise = (i-1, l, r++[x])

split'' :: [a] -> Int -> ([a], [a])
split'' xs n = (\(i, l, r) -> (l, r)) $ foldr helper ((length xs) - n, [], []) xs
  where helper x (i, l, r)
          | i > 0     = (i-1, l, x:r)
          | otherwise = (i-1, x:l, r)
