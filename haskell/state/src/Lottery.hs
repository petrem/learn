module Lottery
  ( drawLottery
  , showLottery
  ) where

import Control.Monad.State
import System.Random

--data Draw = Empty | Ball Int
--newtype Lottery = (Draw, Draw)
type Lottery = (Int, Int)


randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

drawBall :: State StdGen Int
drawBall = makeBall <$> randomSt
  where makeBall x = x `mod` 49 + 1

drawLottery :: State StdGen Lottery
drawLottery = do
  b1 <- drawBall
  b2 <- drawBall
  return (b1, b2)

showLottery :: Lottery -> String
showLottery (b1, b2) = show b1 ++ ", " ++ show b2
