module Stack
  ( stackManip
  , showStackOp
  ) where


import Control.Monad.State

-- newtype State s a = State { runState :: s -> (a, s)}

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push = \x -> state $ \xs -> ((), x:xs)


stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  b <- pop
  push $ a * b
  pop

showStackOp :: State Stack Int -> Stack -> String
showStackOp op s = let (r, s') = runState op s in "Result: " ++ show r ++ " and stack: " ++ show s'
