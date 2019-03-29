module Diamond (diamond) where


import qualified Data.Char as C


diamond :: Char -> Maybe [String]
diamond c
  | not (C.isAsciiUpper c) = Nothing
  | otherwise = Just (_diamond c)

_diamond :: Char -> [String]
_diamond letter = [lineOf c | c <- ['A'..pred letter] ++ [letter,pred letter..'A']]
  where
    offset c = C.ord c - C.ord 'A'
    displacement c = offset letter - offset c
    wing c = replicate (displacement c) ' '
    middle c
      | c == 'A' = "A"
      | otherwise = [c] ++ (replicate (2 * offset c - 1) ' ') ++ [c]
    lineOf c = (wing c) ++ (middle c) ++ (wing c)
