-- hanoi :: (Integral t) => t -> [[t], [t], [t]]

type Tower = [Int]
type Towers t = (t, t, t)

t1 (t, _, _) = t
t2 (_, t, _) = t
t3 (_, _, t) = t

tuplify3 [x1, x2, x3] = (x1, x2, x3)

towerN towers n  -- todo: is there a better way?
  | n == 1 = t1 towers
  | n == 2 = t2 towers
  | n == 3 = t3 towers

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
