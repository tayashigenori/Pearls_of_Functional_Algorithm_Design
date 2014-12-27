
import Data.Array

matches :: Eq a => [a] -> [a] -> Int

matches ws                = test m . scanl step (0, [ ])
  where
    test j [ ]            = [ ]
    test j ((n, sx): nxs) | i == m     = n: test k (drop (k - 1) nxs)
                          | m - k <= i = test k (drop (k - 1) nxs)
                          | otherwise  = test m (drop (k - 1) nxs)
                            where
                              i' = llcp sw (take j sx)
                              i  = if i' == j then m else i'
                              k  = a ! i
    (sw, m)               = (reverse ws, length ws)
    a                = accumArray min m (0, m) (vks ++ vks')
      where
        m            = length sw
        vks          = zip (allcp' sw) [1..m]
        vks'         = zip [m, m - 1 .. 1] (foldr op [ ] vks)
        op (v, k) ks = if v + k == m then k: ks else head ks: ks

step (n, sx) x = (n + 1, x: sx)

allcp xs = [llcp xs (drop k xs) | k <- [0 .. length xs - 1]]
allcp' xs = tail (allcp xs) ++ [0]

llcp xs  [ ]        = 0
llcp [ ] ys         = 0
llcp (x: xs) (y:ys) = if x == y then 1 + llcp xs ys else 0

