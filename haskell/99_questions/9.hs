-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:takeWhile (==x) xs) : (pack $ dropWhile (==x) xs)

pack' :: (Eq a) => [a] -> [[a]]
pack' = foldr packer []
  where packer x [] = [[x]]
        packer x (xs@(y:ys):zs)
          | x == y = (x:xs):zs
          | otherwise = [x]:xs:zs
