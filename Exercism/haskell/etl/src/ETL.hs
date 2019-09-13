module ETL (transform) where

import           Data.Map (Map, fromList, toList)

import           Data.Text (Text, toLower, unpack)

transform :: Map Int Text -> Map Char Int
transform  = fromList . concatMap (\(x, ys) -> [(y, x) | y <- unpack . toLower $ ys]) . toList
