-- Extract a slice from a list.

-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.

slice :: [a] -> Int -> Int -> [a]
slice l i k = drop (i-1) . take k $ l

slice' :: [a] -> Int -> Int -> [a]
slice' l i k = fst $ splitAt (k-i+1) $ snd $ splitAt (i-1) l
