-- Extract a given number of randomly selected elements from a list.
-- λ> rnd_select "abcdefgh" 3 >>= putStrLn
-- eda

import System.Random
import System.Environment


deleteIndexes :: [Int] -> [a] -> [a]
deleteIndexes is xs = map snd $ filter fst $ zip markers xs
  where markers = [if i `elem` is then False else True |i <- [0..]]


selectIndexes :: [Int] -> [a] -> [a]
selectIndexes is xs = (map snd) . (filter fst) $ zip markers xs
  where markers = [if i `elem` is then True else False |i <- [0..]]


deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt k xs = (++) <$> take k <*> drop (k+1) $ xs


maybeIndex :: Int -> [a] -> Maybe a
maybeIndex _ [] = Nothing
maybeIndex k xs
  | k < 0 = Nothing
  | k >= length xs = Nothing
  | otherwise = Just (xs !! k)


extractAt :: Int -> [a] -> (Maybe a, [a])
extractAt = curry $ (,) <$> uncurry maybeIndex <*> uncurry deleteAt


_rnd_select _ 0 _ rs = rs
_rnd_select gen n xs rs =
  case extraction of Nothing  -> rs
                     (Just x) -> _rnd_select newGen (n - 1) remaining (x:rs)
  where
    (rnd, newGen) = randomR (0, max 0 (length xs - 1)) gen
    (extraction, remaining) = extractAt rnd xs

rnd_select :: Show a => Int -> [a] -> IO String
rnd_select n xs = do
  gen <- newStdGen
  return $ show $ _rnd_select gen n xs []



strToInt :: String -> Int
strToInt s = read s :: Int

type ErrorMessage = String
type ParsedArgs = Either ErrorMessage (Int, [String])

parseArgs :: [String] -> ParsedArgs
parseArgs (n:xs) = Right (strToInt n, xs)
parseArgs _ = Left "No arguments. Giving up (cowardly)."


main1 = do
  -- select with repetition (?)
  Right (how_many, list) <- parseArgs <$> getArgs
  gen <- getStdGen
  let randomIndexes = take how_many $ randomRs (0, max 0 (length list - 1)) gen :: [Int]
  let result = selectIndexes randomIndexes list
  putStrLn $ show (how_many, randomIndexes, result)


main = do
    Right (how_many, list) <- parseArgs <$> getArgs
    result <- rnd_select how_many list
    putStrLn result
