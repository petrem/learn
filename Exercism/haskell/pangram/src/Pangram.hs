module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)
import Data.List (group, nub, sort)
import qualified Data.Set as Set

import Control.Exception
import Control.Monad
import System.Time.Extra


isPangram1 :: String -> Bool
isPangram1 = (26 == ) . length . nub . sort . map toLower . filter isAlpha

-- getting rid of nub, need only to remove successive duplicates
isPangram2 :: String -> Bool
isPangram2 = (26 == ) . length . (map head . group) . sort . map toLower . filter isAlpha


-- likewise, using Sets
isPangram :: String -> Bool
isPangram = (26 == ) . Set.size . Set.fromList . map toLower . filter isAlpha



benchmark xs = do
    n <- evaluate $ length xs
    (t1,_) <- duration $ evaluate $ isPangram xs
    (t2,_) <- duration $ evaluate $ isPangram2 xs
    putStrLn $ show n ++ "," ++ show t1 ++ "," ++ show t2

main = do
    forM_ [0,100..10000] $ \i -> benchmark $ take (26 * i) $ cycle ['a'..'z']
    forM_ [0,100..10000] $ \i -> benchmark $ replicate i 'a'
