-- Drop every N'th element from a list.

dropEvery :: Int -> [a] -> [a]
dropEvery n = _helper 0
  where
    _helper _ [] = []
    _helper m (x:xs)
      | n-1 == m = _helper 0 xs
      | otherwise = x:_helper (m+1) xs

dropEvery' :: Int -> [a] -> [a]
dropEvery' n xs = compress xs $ map (==0) $ concat $ repeat [0..n-1]

compress :: [a] -> [Bool] -> [a]
compress xs ss = foldr (\(x,s) acc -> if s then x:acc else acc) [] $ zip xs ss


-- Modulo group

data Modulo a = Modulo a a deriving Show
instance Integral a => Num (Modulo a) where
  (Modulo n x) + (Modulo m y)
    | n /= m = errorWithoutStackTrace "Cannot operate in different moduli"
    | otherwise = Modulo n ((x + y) `mod` n)

  (Modulo n x) - (Modulo m y)
    | n /= m = errorWithoutStackTrace "Cannot operate in different moduli"
    | otherwise = Modulo n (abs (x - y) `mod` n)

  (Modulo n x) * (Modulo m y)
    | n /= m = errorWithoutStackTrace "Cannot operate in different moduli"
    | otherwise = Modulo n ((x * y) `mod` n)

  abs (Modulo n x) = errorWithoutStackTrace "abs undefined on Modulo"
  negate (Modulo n x) = errorWithoutStackTrace "negate undefined on Modulo"
  signum (Modulo n x) = errorWithoutStackTrace "signum undefined on Modulo"
  fromInteger _ = errorWithoutStackTrace "fromInteger undefined on Modulo"


instance (Integral a, Show a) => Enum (Modulo a) where
  fromEnum (Modulo n x)
    | x >= n = errorWithoutStackTrace $ show x ++ " too large for Modulo " ++ show n
    | otherwise = fromEnum x
