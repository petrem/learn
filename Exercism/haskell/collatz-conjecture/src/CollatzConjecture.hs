module CollatzConjecture (collatz) where

hailstone :: Integer -> Integer
hailstone x
  | x == 1    = 0
  | even x    = x `quot` 2
  | otherwise = x * 3 + 1

hailstoneSteps :: Integer -> [Integer]
hailstoneSteps x = takeWhile (>= 1) $ iterate hailstone x

collatz :: Integer -> Maybe Integer
collatz x
  | x <= 0    = Nothing
  | otherwise = Just $ (toInteger . length . hailstoneSteps $ x) - 1


-- Initial, recursive, version
collatz' :: Integer -> Maybe Integer
collatz' x
  | x <= 0    = Nothing
  | otherwise = (Just . steps) x
  where steps y
          | y == 1    = 0
          | even y    = 1 + steps (y `quot` 2)
          | otherwise = 2 + steps ((y * 3 + 1) `quot` 2)


