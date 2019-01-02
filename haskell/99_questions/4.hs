-- Find the number of elements of a list.

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength1 :: [a] -> Int
myLength1 xs = fst $ last $ zip [1..] xs

myLength2 :: [a] -> Int
myLength2 xs = foldl (\x y -> x + 1) 0 xs

-- neat stuff copied from solutions
myLength3 =  foldr (\_ -> (+1)) 0
myLength5 =  foldr (const (+1)) 0
myLength6 =  foldl (const . (+1)) 0
