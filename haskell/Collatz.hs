module CollatzConjecture (collatz) where

import Data.Monoid (Sum)
import Control.Monad (filterM)
import Control.Monad.Extra (iterateM)

-- This version uses iterate, still skip one step for odd numbers in the series
-- data ResultAndSteps a b = ResultAndSteps {result :: a
--                                          ,steps  :: b
--                                          } deriving Show
-- instance Functor (ResultAndSteps a) where
--   fmap f (ResultAndSteps r s) = ResultAndSteps r (f s)

-- instance (Num b) => Semigroup (ResultAndSteps a b) where
--   x <> y = ResultAndSteps (result y) (steps x + steps y)

-- instance Applicative (ResultAndSteps a) where
--   pure x = ResultAndSteps x 0
--   f <*> x = ResultsAndSteps (result f $ result x) (steps x)

-- instance Monad (ResultAndSteps a) where
--   return x = ResultAndSteps x 0
--   x >>= f = f $ result x

-- hailstoneSteps :: Integer -> [Steps Integer]
-- hailstoneSteps = iterate . hailstone

-- hailstone :: (Integral a) => Integer -> ResultAndSteps Integer a
-- hailstone x
--   | x == 1 = ResultAndSteps 1 0
--   | even x = ResultAndSteps (x `quot` 2) 1
--   | otherwise = ResultAndSteps ((x * 3 + 1) `quot` 2) 2

-- ----------------------

data Steps a = Steps { result :: a, steps :: Sum Integer } deriving Show

instance Functor Steps where
  fmap f (Steps r s) = Steps (f r) s

instance Applicative Steps where
  pure r = Steps r 0
  (Steps f _) <*> (Steps r s) = Steps (f r) s

instance Monad Steps where
  return r = Steps r 0
  (Steps r _) >>= f = f r

-- collatz :: Integer -> Maybe Integer
-- collatz x
--   | x <= 0    = Nothing
--   | otherwise = (Just . steps) x

