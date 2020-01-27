import Data.Bits (testBit, shiftR)
import Control.Monad.State

-- check if even, `quot` 2 are faster than testBit/shiftR

shiftRight :: Int -> (Int, Int)
shiftRight x = (length . takeWhile isEven $ divisions, head . dropWhile isEven $ divisions)
  where divisions = iterate (flip shiftR 1) $ x
        isEven = not . (flip testBit 0)

computation :: Int -> State Int Int
computation x = do n <- get
                   let (steps, result) = shiftRight x
                   put (steps)
                   return result

shiftRightState :: Int -> State Int Int
shiftRightState x = runState (computation x) 0

