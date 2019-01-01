--  Find the last but one element of a list.

myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [_] = error "No butts for singletons"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

myButLast1 xs = fst $ last $ zip xs (drop 1 xs)
