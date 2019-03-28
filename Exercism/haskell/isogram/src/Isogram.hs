module Isogram (isIsogram) where

import qualified Data.Char as C
import qualified Data.Set  as S

isIsogram :: String -> Bool
isIsogram =  areUnique . (filter C.isAlphaNum) . (map C.toLower)

areUnique :: String -> Bool
areUnique = (==) <$> (length . id) <*> (length . S.fromList)
