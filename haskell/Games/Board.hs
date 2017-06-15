module Board
  ( Board
  , makeEmptyBoard
  , getAt
  , setAt
  ) where

import qualified Data.List as List
import qualified Data.Map as Map

data Square a = Empty | Square a deriving (Eq, Show)
type Row a = [Square a]
type Squares a = [Row a]

data Board a = Board
  { rows :: Int
  , cols :: Int
  , squares :: Squares a
  } deriving (Show)

type Move a = ((Int, Int), Square a)

-- instance Show a => Show (Board a) where
--   show board = (
--     List.intercalate "\n" (showRows (squares board))) ++ "\n"
--     where showRows row = [foldl (++) "" $ map show x | x <- row]

makeEmptyBoard r c = Board { rows=r
                           , cols=c
                           , squares=replicate r $ replicate c Empty}

makeBoard r c ss = Board { rows=r
                         , cols=c
                         , squares=ss}


-- get a string representation
--showBoard :: Show a => Board a -> [Char]
--showBoard board = List.intercalate "\n" $ show $ squares board

--getGroups :: Eq a => [a] -> [(a, Int)]
--getGroups xs = map (\ys -> (head ys, length ys)) $ List.group xs

-- count squares by content, return a map elem -> count
--elemCounts :: Board a -> Map.Map a Int
--elemCounts board = length $ filter (== c) $ List.intercalate [] board

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = error "Cannot replace in empty list"
replace x y ys
  | x < 0 = error "Negative index"
  | x >= length ys =  error "Index too large"
  | otherwise = take x ys ++ y : drop (x+1) ys

getAt :: Board a -> (Int, Int) -> Square a
getAt board (x,y) = ((squares board) !! x ) !! y

setAt :: Board a -> (Int, Int) -> Square a -> Board a
setAt board (x,y) s =
  let row = (squares board) !! y in
    Board { rows = rows board
          , cols = cols board
          , squares = replace y (replace x s row) (squares board)}

applyMoves :: [Move a] -> Board a -> Board a
applyMoves [] board = board
applyMoves moves board = foldl (\b (pos, s) -> setAt b pos s) board moves


allBoardPositions :: Board a -> [(Int, Int)]
allBoardPositions board = [(x,y) | x <- [0..rows board - 1],
                                   y <- [0..cols board - 1]]

availablePositions :: Eq a => Board a -> [(Int, Int)]
availablePositions board = filter ((Empty ==) . (getAt board)) (allBoardPositions board)

