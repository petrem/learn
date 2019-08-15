module DNA (nucleotideCounts, Nucleotide(..)) where

import Control.Applicative (liftA3)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Text.Read (readEither)


data Nucleotide = A | C | G | T deriving (Eq, Ord, Read, Show)


nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = foldl updateCounts (Right zeroCounts)

zeroCounts :: Map Nucleotide Int
zeroCounts = Map.fromList [(A,0), (C,0), (G,0), (T,0)]

updateCounts :: Either String (Map Nucleotide Int) -> Char -> Either String (Map Nucleotide Int)
updateCounts m c = liftA3 (Map.insertWith (+)) (readEither [c]) (Right 1) m
