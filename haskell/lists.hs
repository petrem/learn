doubleMe :: Integer -> Integer
doubleMe x = x + x

doubleSmallNumber x = if x > 100 then x else x*2

doubleSmallNumber' x = succ (if x > 100 then x else x*2)

-- quicksort l = if length L == 0
--               then []
--               else
--                 quicksort (filter (lambda x = x < head l) (tail l)) ++
--                 [head l] ++
--                 quicksort (filter (lambda x = x >= head l) (tail l))




quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y < x] ++
                   x :
                   quicksort [ y | y <- xs, y >= x ]



quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort (let isSmaller y = y < x in filter isSmaller xs) ++
                    x :
                    quicksort (let isLargerOrEqual y = y >= x in filter isLargerOrEqual xs)


quicksort'' :: Ord a => [a] -> [a]
quicksort'' [] = []
quicksort'' (x:xs) = quicksort (filter ((<x)) xs) ++ x : quicksort (filter (>=x) xs)
 

selectsort :: Ord a => [a] -> [a]
selectsort [] = []
selectsort [x] = [x]
selectsort (h:t)
  | h <= mini = h : selectsort t
  | otherwise = mini : selectsort (beforeMin ++ h : afterMin)
  where mini = minimum t
        minPos = head [b | (a, b) <- zip t [0,1..], a == mini]
        beforeMin = take minPos t
        afterMin = drop (minPos + 1) t


addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' a b = (fst a + fst b, snd a + snd b)

addVectors :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)


mymax :: (Ord a) => [a] -> a
mymax [] = error "nothing to see here"
mymax [x] = x
mymax (x:xs) =
  let tailmax = mymax xs in
    if tailmax > x then tailmax else x


replicate' :: (Integral a, Eq a) => a -> b -> [b]
replicate' 0 _ = []
replicate' r xs
  | r > 0 = xs : replicate' (r-1) xs
  | otherwise = error "how would you do that?"


take' :: (Integral i, Eq i) => i -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs)
  | n > 0 = x : take' (n-1) xs
  | otherwise = error "troll!"


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


repeat' :: a -> [a]
repeat' x = x : repeat' x


elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
  | e == x = True
  | otherwise = elem' e xs


elemAt :: (Integral i, Eq i) => i -> [a] -> a
elemAt _ [] = error "Index out of range"
elemAt n (x:xs)
  | n < 0 = error "Index out of range"
  | n == 0 = x
  | otherwise = elemAt (n-1) xs


uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x1:x2:xs) = (if x1 == x2 then [] else [x1]) ++ uniq (x2:xs)


zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


zipWith'' :: (a ->b ->c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = let f' (x,y) = f x y in
                      map f' (zip xs ys)

-- todo
-- zipLongest :: [a] -> [b] -> [(Maybe a, Maybe b)]
-- zipLongest [] [] = []
-- zipLongest (x:xs) [] = (x, Nothing) : zipLongest xs []
-- zipLongest [] (y:ys) = (Nothing, y) : zipLongest [] ys
-- zipLongest (x:xs) (y:ys) = (x,y) : zip' xs ys


-- todo
-- slice :: (Integral i, Eq i) => i -> i -> [a] -> [a]
-- slice 0 0 _ = []
-- slice _ _ [] = []
-- slice 0 stop (x:xs) = x : slice 0 (stop-1) xs
-- slice start 0 _ = []
-- slice start stop (x:xs) = 


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ i [] = i
foldr' f i (x:xs) = f x (foldr' f i xs)


foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ i [] = i
foldl' f i (x:xs) = f (foldl' f i xs) x


flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = let g x y = f y x in g

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x -> \y -> f y x

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs


split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split x ys = takeWhile (/= x) ys : split x (dropWhile (== x) (dropWhile (/= x) ys))


splitBy :: (Eq a) => [a] -> [a] -> [[a]]
splitBy [] xs = [xs]
splitBy _ [] = []
splitBy xs ys = takeWhile (`notElem` xs) ys : splitBy xs (dropWhile (`elem` xs) (dropWhile (`notElem` xs) ys))
