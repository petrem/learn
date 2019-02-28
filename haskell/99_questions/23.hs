-- Extract a given number of randomly selected elements from a list.
-- λ> rnd_select "abcdefgh" 3 >>= putStrLn
-- eda

import System.Random
import System.Environment


-- without '>>=' stuff that i have no clue about at this point

deleteIndexes :: [Int] -> [a] -> [a]
deleteIndexes is xs = map snd $ filter fst $ zip markers xs
  where markers = [if i `elem` is then False else True |i <- [0..]]


strToInt :: String -> Int
strToInt s = read s :: Int


parseArgs :: [String] -> (Int, [String])
parseArgs (n:xs) = (strToInt n, xs)
parseArgs _ = errorWithoutStackTrace "No arguments. Giving up (cowardly)."


main = do
  (how_many, list) <- parseArgs <$> getArgs
  gen <- getStdGen
  let randomIndexes = take how_many $ randomRs (0, length list - 1) gen :: [Int]
  let result = deleteIndexes randomIndexes list
  putStrLn $ show (how_many, randomIndexes, result)
