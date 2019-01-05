import qualified Data.Time.Clock as Clock

-- Reverse a list.


main = do
  -- times are (interpreted in ghci, compiled)
  -- reverse: 0.00991s, 0.015118s
  t1 <- Clock.getCurrentTime
  putStrLn (seq (length $ reverse [1..10**5]) "reverse:")
  t2 <- Clock.getCurrentTime
  putStrLn (show $ Clock.diffUTCTime t2 t1)

  -- myReverse: 667.068931s, 457.225414s
  t1 <- Clock.getCurrentTime
  putStrLn (seq (length $ myReverse [1..10**5]) "myReverse:")
  t2 <- Clock.getCurrentTime
  putStrLn (show $ Clock.diffUTCTime t2 t1)

  -- myReverse1: 645.656359s, 359.032194s
  t1 <- Clock.getCurrentTime
  putStrLn (seq (length $ myReverse1 [1..10**5]) "myReverse1:")
  t2 <- Clock.getCurrentTime
  putStrLn (show $ Clock.diffUTCTime t2 t1)

  -- myReverse2: 0.011004s, 0.013771s
  t1 <- Clock.getCurrentTime
  putStrLn (seq (length $ myReverse2 [1..10**5]) "myReverse2:")
  t2 <- Clock.getCurrentTime
  putStrLn (show $ Clock.diffUTCTime t2 t1)

  -- myReverse3: 0.050446s, 0.013349s
  t1 <- Clock.getCurrentTime
  putStrLn (seq (length $ myReverse3 [1..10**5]) "myRreverse3:")
  t2 <- Clock.getCurrentTime
  putStrLn (show $ Clock.diffUTCTime t2 t1)


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

-- still quite fast (~ 5x native, though;
-- update: turns out it is also just as fast, when compiled and not interpreted
myReverse3 :: [a] -> [a]
myReverse3 = myFoldl (myFlip (:)) []
  where myFlip f x y = f y x
        myFoldl :: (a -> b -> a) -> a -> [b] -> a
        myFoldl f i [x] = f i x
        myFoldl f i (x:xs) = myFoldl f (f i x) xs

-- copied from solutions:
reverse4 :: [a] -> [a]
reverse4 list = reverse' list []
  where
    reverse' [] reversed     = reversed
    reverse' (x:xs) reversed = reverse' xs (x:reversed)
