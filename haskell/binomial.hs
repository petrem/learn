module Binomial (binomial) where

binomial :: Integer -> Integer -> Integer
binomial n 1 = n
binomial n k
  | n == k    = 1
  | n < k     = error "Binomial coefficient not defined for n < k"
  | otherwise = (binomial (n-1) (k-1)) + (binomial (n-1) k)
