import qualified Data.List as L


-- foo :: [a] -> Int -> [([a], [a])]
-- foo [] _ = [([], [])]
-- foo (x:[]) _ = [([x], [])]
-- foo xs 1 = [(xs, [])]
-- foo xs k = L.map (\(hs, ts) -> (hs,  foo ts (k-1))) partitions
--   where partitions = [L.splitAt i xs | i <- [1, L.length xs - 1]]

partitionDisplacements :: (Integral a) => a -> [[a]]
partitionDisplacements 0 = []
partitionDisplacements n = n : partitionDisplacements (n-1)


l = [1,2,3,4]
-- [[1,]
