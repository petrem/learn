import Data.Time.Clock

-- Reverse a list.


timeIt = do
  -- reverse: 0.00991s
  t1 <- Data.Time.Clock.getCurrentTime
  putStrLn (seq (length $ reverse' [1..10**5]) "reverse:")
  t2 <- Data.Time.Clock.getCurrentTime
  putStrLn (show $ Data.Time.Clock.diffUTCTime t2 t1)

  -- myReverse: 667.068931s
  t1 <- Data.Time.Clock.getCurrentTime
  putStrLn (seq (length $ myReverse [1..10**5]) "myReverse:")
  t2 <- Data.Time.Clock.getCurrentTime
  putStrLn (show $ Data.Time.Clock.diffUTCTime t2 t1)

  -- myReverse1: 645.656359s
  t1 <- Data.Time.Clock.getCurrentTime
  putStrLn (seq (length $ myReverse1 [1..10**5]) "myReverse1:")
  t2 <- Data.Time.Clock.getCurrentTime
  putStrLn (show $ Data.Time.Clock.diffUTCTime t2 t1)

  -- myReverse2: 0.011004s
  t1 <- Data.Time.Clock.getCurrentTime
  putStrLn (seq (length $ myReverse2 [1..10**5]) "myReverse2:")
  t2 <- Data.Time.Clock.getCurrentTime
  putStrLn (show $ Data.Time.Clock.diffUTCTime t2 t1)

  -- myReverse3: 0.050446s
  t1 <- Data.Time.Clock.getCurrentTime
  putStrLn (seq (length $ myReverse3 [1..10**5]) "myRreverse3:")
  t2 <- Data.Time.Clock.getCurrentTime
  putStrLn (show $ Data.Time.Clock.diffUTCTime t2 t1)


-- native
reverse' = reverse

-- slow
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- slow
myReverse1 :: [a] -> [a]
myReverse1 [] = []
myReverse1 xs = last xs:myReverse1 (init xs)

-- friggin' fast (almost as native reverse)
myReverse2 :: [a] -> [a]
myReverse2 = foldl (flip (:)) []

-- still quite fast (~ 5x native, though)
myReverse3 :: [a] -> [a]
myReverse3 = myFoldl (myFlip (:)) []
  where myFlip f x y = f y x
        myFoldl :: (a -> b -> a) -> a -> [b] -> a
        myFoldl f i [x] = f i x
        myFoldl f i (x:xs) = myFoldl f (f i x) xs

-- copied from solutions:
reverse :: [a] -> [a]
reverse list = reverse' list []
  where
    reverse' [] reversed     = reversed
    reverse' (x:xs) reversed = reverse' xs (x:reversed)
