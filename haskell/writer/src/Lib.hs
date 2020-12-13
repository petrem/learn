module Lib
    ( someFunc
    ) where

import Control.Monad.Writer

someFunc :: IO ()
someFunc = putStrLn (getResult applyAll)

getResult :: (Show a) => Writer [String] a -> String
getResult result = let (x, l) = runWriter result in "Got result: " ++ show x ++ " with log:\n" ++ unlines l


fn1 :: Num a => a -> a
fn1 x = x + 1

fn2 :: Num a => a -> a
fn2 x = x * 2

applyWithLog :: (a -> a) -> b -> [a] -> Writer [b] [a]
applyWithLog f l xs = writer (fmap f xs, [l])

applyAll :: (Show a, Num a) => Writer [String] [a]
applyAll =  applyWithLog fn1 "fn1" [1,2,3] >>= applyWithLog fn1 "fn1" >>= applyWithLog fn2 "fn2"
