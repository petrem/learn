-- friggin' ugly...
degToMinSecRounded x = normalize $ wholePartsWithRoundLast $ take 3 (degToMinSec_helper x)
  where degToMinSec_helper y = y : degToMinSec_helper (decimal*60)
          where decimal = y - fromIntegral (truncate y)
        wholePartsWithRoundLast ys = map truncate (init ys) ++ [round (last ys)]
        normalize ys = reverse (roundUp (reverse ys) [])
        roundUp [y] rounded = rounded ++ [y]
        roundUp (y:z:xs) rounded = rounded ++ [if y >= 60 then 0 else y] ++ (roundUp ((if y >= 60 then z+1 else z):xs) [])


-- todo: better implement normalize/roundup
-- todo: decimal tends to be imprecise e.g. decimal 1.1 tends to be 1.10000...9; is there a better way?
