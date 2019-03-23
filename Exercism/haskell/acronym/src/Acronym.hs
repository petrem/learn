module Acronym (abbreviate) where


import qualified Data.Char as C
import qualified Data.List as L
-- import qualified Data.List.Split as S


-- abbreviate :: String -> String
-- abbreviate = concat . (map acronymize) . (map (filter C.isAlpha)) . words . toUpperFirst
--   where acronymize = (map head) . S.split (S.condense . S.dropDelims . S.dropBlanks $ S.whenElt C.isLower)
--         toUpperFirst = (:) <$> C.toUpper . head <*> tail

-- abbreviate :: String -> String
-- abbreviate = concat . (map acronymize) . words
--   where acronymize = (++) <$> (:[]) . C.toUpper . head <*> (filter C.isUpper) . tail

toUpperHead :: String -> String
toUpperHead = (:) <$> C.toUpper . head <*> tail

isAllCaps :: String -> Bool
isAllCaps = all C.isUpper

-- unclear case: "GNULinux" should become "G", "GL", what?
abbreviate :: String -> String
abbreviate = filter C.isUpper . L.concatMap (toUpperHead . abbrevAllCaps) . words . cleanUp
  where
    cleanUp = foldr f []
      where f c acc
              | c == '\'' = acc
              | C.isPunctuation c = ' ':acc
              | otherwise = c:acc
    abbrevAllCaps xs = if isAllCaps xs then take 1 xs else xs

