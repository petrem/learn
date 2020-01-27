import Data.Char
import Data.List

newtype Position b a = Position {getPosition :: (a, b)}

instance Functor (Position b) where
  fmap f (Position (col, row)) = Position (f col, row)

columnLetter :: Int -> Char
columnLetter = chr . (ord 'a' +) . pred

instance (Show i1, Integral i2, Show i2) => Show (Position i1 i2) where
  show (Position (col, row)) = (columnLetter . fromIntegral $ col) : show row


data Direction = Direction Int Int
data Move = Move { repeat :: Bool
                 , directions :: [Direction]
                 , steps :: [Int]}

knightMoves :: (Integral a) => Position a a -> [Position a a]
knightMoves (Position (c, r)) = [Position (c+i, r+j) | i <- [-2..2] \\ [0], j <- [-2..2] \\ [0], (abs i == 2) /= (abs j == 2), c + i `elem` [1..8], r + j `elem` [1..8]]


canReachInGivenMoves :: Integral a => Int -> Position a a -> Position a a -> Bool
canReachInGivenMoves n (Position (c, r)) (Position (c', r')) = False

-- canReachIn3 :: Integral a => Position a a -> Position a a -> Bool
-- canReachIn3 start stop = do
--   move1 <- knightMoves start
--   move2 <- k
