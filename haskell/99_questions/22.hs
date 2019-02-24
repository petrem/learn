-- Create a list containing all integers within a given range.
-- range 4 9
-- [4,5,6,7,8,9]

-- obvious solution
range :: Integral i => i -> i -> [i]
range start stop = [start..stop]

range' :: Integral a => a -> a -> [a]
range' start stop = drop (fromIntegral start) . take (fromIntegral stop) $ countingFrom 0
  where countingFrom i = i:countingFrom (i+1)
