module Lexer (tokenize) where

import qualified Data.Char as Char


data Position = Position { posLine::Int, posCol::Int } deriving (Eq, Show)
instance Ord Position where
  compare pos1 pos2 = (posLine pos1 `compare` posLine pos2) `mappend` (posCol pos1 `compare` posCol pos2)

startPosition = Position {posLine = 1, posCol = 1}

advancePos :: Position -> Int -> Int -> Position
advancePos pos l c
  | l < 0 || c < 0 = errorWithoutStackTrace "Cannot advance by negative numbers"
  | otherwise      = Position {posLine = posLine pos + l, posCol = if l > 0 then 1 else posCol pos + c}

-- TODO: maybe I could define a Monad where the position would be incremented, as context?

data Token =
  Plus { getPos :: Position } |
  Minus { getPos :: Position } |
  Mul { getPos :: Position } |
  Div { getPos :: Position } |
  Id {getPos::Position, getId :: String } |
  Num {getPos :: Position, getNum :: Integer }
  deriving (Eq, Show)


skipSpace :: String -> Position -> (String, Position)
skipSpace "" pos = ("", pos)
skipSpace input@(x:xs) pos
  | x == '\n'      = skipSpace xs (advancePos pos 1 0)
  | Char.isSpace x = skipSpace xs (advancePos pos 0 1)
  | otherwise      = (input, pos)

collectId :: String -> Position -> (String, Position, Maybe Token)
collectId input pos = ( dropWhile Char.isAlphaNum input
                      , advancePos pos 0 (length lexeme)
                      , Just $ Id pos lexeme)
  where lexeme = takeWhile Char.isAlphaNum input

collectNum :: String -> Position -> (String, Position, Maybe Token)
collectNum input pos = ( dropWhile Char.isDigit input
                       , advancePos pos 0 (length lexeme)
                       , Just $ Num pos (read lexeme :: Integer))
  where lexeme = takeWhile Char.isAlphaNum input

getNextToken :: String -> Position -> (String, Position, Maybe Token)
getNextToken "" pos = ("", pos, Nothing)
getNextToken input@(peek:_) pos
  | Char.isLetter peek = collectId input pos
  | Char.isDigit peek = collectNum input pos


tokenize :: String -> [Token]
tokenize s = 
