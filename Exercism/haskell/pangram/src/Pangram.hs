module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)
import Data.List (nub, sort)

isPangram :: String -> Bool
isPangram = (26 == ) . length . nub . sort . (map toLower) . filter isAlpha
