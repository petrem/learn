module DNA (nucleotideCounts, Nucleotide(..)) where

import Control.Applicative (liftA3)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Text.Read (readEither)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Read, Show)
type NucleotideCounts = Map Nucleotide Int

nucleotideCounts :: String -> Either String NucleotideCounts
nucleotideCounts = foldl updateCounts (Right zeroCounts)

zeroCounts :: NucleotideCounts
zeroCounts = Map.fromList [(A,0), (C,0), (G,0), (T,0)]

updateCounts :: Either String NucleotideCounts -> Char -> Either String NucleotideCounts
updateCounts m c = liftA3 (Map.insertWith (+)) (readEither [c]) (Right 1) m
