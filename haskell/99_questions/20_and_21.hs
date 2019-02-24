--20.  Remove the K'th element from a list
-- Example: removeAt 2 "abcd"
-- ('b',"acd")

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n-1), (\(l,r) -> take (n-1) l ++ r) $ splitAt n xs)

-- same, rewritten; if I could get the main definition to be point-free, all if this would be
-- point-free.
-- to revisit this after reading about Monads, esp. the Reader monad
-- see also: https://stackoverflow.com/questions/11709350/applying-multiple-functions-to-the-same-value-point-free-style-in-haskell
removeAt' :: Int -> [a] -> (a, [a])
removeAt' n xs = (,) (left xs) (right xs)
  where
    left = flip (!!) (n-1)
    right = (uncurry $ (++) . (take (n-1))) . (splitAt n)

-- 21. Insert an element at a given position into a list.

insertAt :: a -> Int -> [a] -> [a]
insertAt x pos = (uncurry $ flip( flip (++) . (x:))) . (splitAt (pos-1))
-- ([1,2], [3,4,5])


-- A funnily convolute version

-- Maybe flavored Dirac/Kronecker delta -like function
maybeDelta :: Int -> a -> Maybe a
maybeDelta x e
  | x == 0    = Just e
  | otherwise = Nothing

flatten :: [(a, Maybe a)] -> [a]
flatten [] = []
flatten ((x, Nothing):xs) = x:flatten xs
flatten ((x, Just y):xs) = x:y:flatten xs

insertAt' :: a -> Int -> [a] -> [a]
insertAt' val pos xs = flatten $ zip xs [maybeDelta (i - pos + 1) val | i<- [0..]]

diracSpinningInGrave :: a -> a -> Int -> a
diracSpinningInGrave x y i = if i == 0 then y else x

insertAt'' :: a -> Int -> [a] -> [a]
insertAt'' val pos xs = flatten $ zip xs [myValWhereIWantIt (i - pos + 1) | i<- [0..]]
  where myValWhereIWantIt = diracSpinningInGrave Nothing (Just val)

insertAt''' :: a -> Int -> [a] -> [a]
insertAt''' val pos xs = flatten $ map (fmap subtituteValInRespectiveTuple) $ zip xs [0..]
  where subtituteValInRespectiveTuple = diracSpinningInGrave Nothing (Just val) . (subtract (pos - 1))
