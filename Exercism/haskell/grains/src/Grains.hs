module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | validSquare n  = Just (2^(n - 1))
  | otherwise      = Nothing
  where validSquare = (&&) <$> (1 <= ) <*> (<= 64)

total :: Integer
total = 18446744073709551615

