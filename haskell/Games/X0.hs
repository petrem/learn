module X0
  (
  ) where

import qualified Data.List as List

import qualified Board

data Player 'x' | '0'

-- Board

X0Board = Board.makeEmptyBoard 3 3
data X0Row = X0Row [Char] deriving (Show)
instance Read X0Row where
  read s = X0Row s

-- Static Scores

gameWinned = 1000

rowScore :: X0Row -> Int

lineScore :: [Char] -> Int
lineScore "..." = 0
lineScore "..0" = -10
lineScore "..x" = 10
lineScore ".0." = -10
lineScore ".00" = -100
lineScore ".0x" = 0
lineScore ".x." = 10
lineScore ".x0" = 0
lineScore ".xx" = 100
lineScore "0.." = -10
lineScore "0.0" = -100
lineScore "0.x" = 0
lineScore "00." = -100
lineScore "000" = -gameWinned
lineScore "00x" = 0
lineScore "0x." = 0
lineScore "0x0" = 0
lineScore "0xx" = 0
lineScore "x.." = 10
lineScore "x.0" = 0
lineScore "x.x" = 100
lineScore "x0." = 0
lineScore "x00" = 0
lineScore "x0x" = 0
lineScore "xx." = 100
lineScore "xx0" = 0
lineScore "xxx" = gameWinned

-- get the main diagonal
diagonAlley :: [[a]] -> [a]
diagonAlley board = diagonAlley' 0 board
  where diagonAlley' :: Int -> [[a]] -> [a]
        diagonAlley' _ [] = []
        diagonAlley' y (x:xs) = x !! y : diagonAlley' (y+1) xs

-- boardScore :: [[Char]] -> Int
-- boardScore board = sum $ map lineScore ( intercalate [] [board, transpose board] ++ [diagonAlley board, diagonAlley (map reverse board)])

-- Play

-- otherPlayer :: Char -> Char
-- otherPlayer 'x' = '0'
-- otherPlayer '0' = 'x'
-- otherPlayer c = error ("No such player: " ++ [c])

-- playerCmp :: Ord a => Char -> (a -> a -> Ordering)
-- playerCmp 'x' = compare
-- playerCmp '0' = flip compare
-- playerCmp c = error ("No such player: " ++ [c])

-- bestNextMoves :: Char -> [[Char]] -> [(Int, ((Int,Int), Char))]
-- bestNextMoves player board = reverse $ sortBy (playerCmp player) [
--   (boardScore (makeMove board nextMove), nextMove) |
--     nextMove <- zip (availableMoves board) (repeat player)]
