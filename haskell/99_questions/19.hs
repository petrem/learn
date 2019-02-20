-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).

--      a   b  c  d
--  <-  b   c  d][a
--  ->  d][ a  b  c

caesar :: [a] -> Int -> [a]
caesar [] _ = []
caesar xs n
  | n > 0     = (drop n' xs) ++ (take n' xs)
  | otherwise = (drop (l-n') xs) ++ (take (l-n') xs)
  where l = length xs
        n' = abs n `mod` l

caesar' :: [a] -> Int -> [a]
caesar' [] _ = []
caesar' xs n = (++) <$> (drop m) <*> (take m) $ xs
  where n' = abs n `mod` length xs
        m = if n < 0 then length xs - n' else n'
