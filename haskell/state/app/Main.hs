module Main where

import System.Random
import Control.Monad.State(runState)

import Stack
import Lottery

main :: IO ()
main = do
  putStrLn $ showStackOp stackManip [2,3,4]
  putStrLn . showLottery . fst $ runState drawLottery (mkStdGen 1234)

