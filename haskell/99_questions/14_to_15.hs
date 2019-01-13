--14. Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []

dupli' :: [a] -> [a]
dupli' xs = [x | x<-xs, _ <- [1,2]]


--15. Replicate the elements of a list a given number of times.

repli :: Int -> [a] -> [a]
repli n = foldr (\x acc -> replicate n x ++ acc) []

repli' :: Int -> [a] -> [a]
repli' n xs = [x | x<-xs, _ <- [1..n]]

repli'' :: Int -> [a] -> [a]
repli'' n = concatMap $ replicate n

repli''' :: Int -> [a] -> [a]
repli''' concatMap . replicate
