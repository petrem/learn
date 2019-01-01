--  Find the last element of a list. 

myLast1 :: [a] -> a
myLast1 [] = error "Empty list"
myLast1 (x:[]) = x
myLast1 (x: xs) = myLast1 xs

myLast2 :: [a] -> a
myLast2 (x:[]) = x
myLast2 xs = foldl1 (\acc x -> x) xs
