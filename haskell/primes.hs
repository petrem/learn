import Data.List

--smallPrimes :: Integral a => [a]
-- the last one MUST be a 6k-1 number for isPrime to function correctly
smallPrimes = [2,3,5,7,11,13,17,19,23,29]


floorSqrt :: (Integral a) => a -> a
floorSqrt x = floor $ sqrt (fromIntegral x)


isDivisibleWithAny x [] = False
isDivisibleWithAny x (y:ys)
  | (x `mod` y) == 0 = True
  | otherwise = isDivisibleWithAny x ys


isPrime x
  | x <= 1 = False
  | x <= 3 = True
  | otherwise = not $ isDivisibleWithAny x primesOrCandidates
  where primesOrCandidates
          | floorSqrt x < last smallPrimes = takeWhile (<= (floorSqrt x)) smallPrimes
          -- 6k+/-1  but starting from a 6k-1
          | otherwise = smallPrimes ++ (foldl1 (++) [[y, y+2] | y <- [last smallPrimes, last smallPrimes + 6 .. floorSqrt x]])


isRelativePrime x y = ((== 1) . (gcd x)) y


-- find small primes upto N using Eratosth2ene's sieve
-- this is copied from https://wiki.haskell.org/Prime_numbers#Sieve_of_Eratosthenes
primesTo m = sieve [2..m]       {- (\\) is set-difference for unordered lists -}
             where
             sieve (x:xs) = x : sieve (xs \\ [x,x+x..m])
             sieve [] = []

-- Euler's totient function
-- number of K, 1 <= K <= x, so that gcd(K, x) = 1
totient x = length $ filter (==1) $ map (gcd x) [1..x]


-- prime numbers theorem: number of primes up to N -> N/log N when N->inf
pnt_pi x = x / log x


-- riemann zeta function
zeta s n = sum $ take n [ x ** (-s) | x <- [1..] ]

-- riemmann hypothesis
-- TODO...

-- primes progressions from 5
primesFrom5To m = drop 2 $ primesTo m

progression step = takeWhile isPrime [5, 5+step ..]
