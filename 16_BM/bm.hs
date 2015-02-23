import Allcp

matches :: Eq a => [a] -> [a] -> [Int]
--matches ws = map length . filter (endswith ws) . inits
--matches "abcab" "ababcabcab" = [7, 10]

-- scan lemma
--map (foldl op e) . inits = scanl op e
--map f . filter p = map fst . filter snd . map (fork (f, p))
--matches ws
--   = map fst . filter snd . map (fork (length, endswith ws)) . inits
--endswith ws = foldl op e

--the tupling law for foldl
--fork (foldl op1 e1, foldl op2 e2) = foldl op (e1, e2)
--fork (length, endswith ws) = foldl step (0, e)
--step (n, x) y              = (n + 1, op x y)
--matches ws = map fst . filter snd . scanl step (0, e)
--endswith ws = p . foldl op e
--map f . filter (p . g)
--  = map fst . filter (p . snd) . map (fork (f, g))
--matches ws = map fst . filter (p . snd) . scanl step (0, e)

step (n, sx) x        = (n + 1, x: sx)

matches ws                = test m . scanl step (0, [])
  where
    test j []             = []
    test j ((n, sx): nxs) | i == m     = n: test k (drop (k - 1) nxs)
                          | m - k <= i = test k (drop (k - 1) nxs)
                          | otherwise  = test m (drop (k - 1) nxs)
                            where
                              i' = llcp sw (take j sx)
                              i  = if i' == j then m else i'
                              k  = shift sw i
    (sw, m)               = (reverse ws, length ws)
    shift sw i            = head [k | k <- [1..m], llcp sw (drop k sw) == min i (m - k)]
