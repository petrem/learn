module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)
import Data.List (group, nub, sort)
import qualified Data.Set as Set

isPangram1 :: String -> Bool
isPangram1 = (26 == ) . length . nub . sort . map toLower . filter isAlpha

-- getting rid of nub, need only to remove successive duplicates
isPangram2 :: String -> Bool
isPangram2 = (26 == ) . length . (map head . group) . sort . map toLower . filter isAlpha


-- likewise, using Sets
isPangram :: String -> Bool
isPangram = (26 == ) . Set.size . Set.fromList . map toLower . filter isAlpha
