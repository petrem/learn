-- hanoi :: (Integral t) => t -> [[t], [t], [t]]

type Tower = [Int]
type Towers t = (t, t, t)

tuplify3 [x1, x2, x3] = (x1, x2, x3)

towerN (t1, t2, t3) n  -- todo: is there a better way?
  | n == 1 = t1
  | n == 2 = t2
  | n == 3 = t3

type TowersWithOrder t o = [(t, o)]

-- a quicksort on towers with order "tag"
--orderedTowers :: TowersWithOrder  => a -> a
orderedTowers [] = []
orderedTowers (x:xs) =
  orderedTowers (let isSmaller y = snd y < snd x in filter isSmaller xs) ++
  x :
  orderedTowers (let isLargerOrEqual y = snd y >= snd x in filter isLargerOrEqual xs)

-- covert back from ordered towers to "normal" towers
--fromOrderedTowers :: =>...
fromOrderedTowers ts = tuplify3 $ removeOrder ts
  where
    removeOrder [] = []
    removeOrder (x:xs) = fst x : removeOrder xs

topDisc xs = last $ takeWhile (/=0) xs

addDisc x xs =
  let existing = takeWhile (/=0) xs in
    existing ++ [x] ++ replicate (length xs - length existing - 1) 0 -- todo: is there a better way?

removeTopDisc xs =
  let existing = takeWhile (/=0) xs in
    init existing ++ replicate (length xs - length existing + 1) 0

-- hanoi' :: ...
hanoi' n s d towers
  | n <= 0 = error "n must be positive"
  | n == 1 = fromOrderedTowers $ orderedTowers
    [ (removeTopDisc $ towerN towers s, s)
    , (addDisc topSourceDisc $ towerN towers d, d)
    , (towerN towers otherTower, otherTower)]
  | otherwise =
      hanoi' (n - 1) otherTower d $ hanoi' 1 s d $ hanoi' (n - 1) s otherTower towers
  where otherTower = 6 - s - d
        topSourceDisc = topDisc $ towerN towers s


--
descending 1 [] = [1]
descending x ys = ys ++ [x] ++ descending (x - 1) []

generateInitial n =
  ( descending n []
  , replicate n 0
  , replicate n 0)


hanoi n s d = hanoi' n s d $ generateInitial n

-- display

mirrored :: [a] -> [a] -> [a]
mirrored xs ys = xs ++ ys ++ reverse xs

spaces :: Int -> [Char]
spaces x = replicate x ' '

-- sides :: (Fractional b) => Int -> [a] -> b
-- sides w xs = (fromIntegral w - fromIntegral (length xs)) / 2

-- width :: Int -> Int
-- width n = 1 + (floor (logBase 10 $ fromIntegral n))

strDisc :: Int -> Int -> [Char]
strDisc n d
  | d == 0 = mirrored (spaces n) (padded "|")
  | otherwise = mirrored (spaces (n - d) ++ replicate d '-') (padded (show d))
  where
    width :: Int
    width = 1 + (floor $ logBase 10 $ fromIntegral n)

    sides :: (Fractional b) => [Char] -> b
    sides cs = (fromIntegral width - fromIntegral (length cs)) / 2

    padded :: [Char] -> [Char]
    padded cs = spaces (ceiling (sides cs)) ++ cs ++ spaces (floor (sides cs))

join _ [] = error "empty list"
join _ [y] = [y]
join x (y:ys) = y : x : join x ys

strTowers towers =
  foldl (++) "" $ join "\n" $ map ((foldl (++) "") . (join " ") . (map (strDisc n))) layers
--  foldl (++) "" $ foldl (++) [] $ map (map (strDisc n)) $ layers
  where
    n = length $ towerN towers 1
    reversedTowers = [reverse $ towerN towers i | i <- [1..3]]
    layers = [map (!! i) reversedTowers | i <- [0..n-1]]
