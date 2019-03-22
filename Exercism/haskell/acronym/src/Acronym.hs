module Acronym (abbreviate) where


import qualified Data.Char as C
import qualified Data.List.Split as S


abbreviate :: String -> String
abbreviate = concat . (map acronymize) . words
  where acronymize = (++) <$> (:[]) . C.toUpper . head <*> foo . (drop 1)
        foo = map head $ S.split (S.condense . S.dropDelims . S.dropBlanks $ S.whenElt C.isLower)

-- fails on all caps word
abbreviate' :: String -> String
abbreviate' = concat . (map acronymize) . words
  where acronymize = (++) <$> (:[]) . C.toUpper . head <*> (filter C.isUpper) . (drop 1)
