-- Remove the K'th element from a list
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
