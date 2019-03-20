digits::(Integral i) => i -> [i]
digits n = reverse . digits_hlp $ abs n
  where digits_hlp n
          | n < 10 = [n]
          | otherwise = n `mod` 10 : digits_hlp (n `div` 10)

leastIntegerWithDigitsAddingUpToFourty = head $ dropWhile ((<40) . sum . digits) [1..]
