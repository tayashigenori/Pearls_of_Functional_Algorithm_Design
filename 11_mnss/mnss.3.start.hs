{-# LANGUAGE NPlusKPatterns #-}

mnss :: [Int] -> Int
extract = map (map fst . filter snd)

-- markings
markings    :: [a] -> [[(a, Bool)]]
markings xs = [zip xs bs | bs <- booleans (length xs)]

booleans 0 = [[]]
booleans (n + 1) = [b: bs | b <- [True, False], bs <- booleans n]

-- nonseg
data State = E | S | M | N
--nonsegs = pick N

-- pick
pick :: State -> [a] -> [[a]]
pick q = extract . filter ((== q) . foldl step E . map snd) . markings

--does not work
pick E xs          = [[]]
pick S []          = []
pick S (xs ++ [x]) = map (++ [x]) (pick S xs ++ pick E xs)
pick M []          = []
pick M (xs ++ [x]) = pick M xs ++ pick S xs
pick N []          = []
pick N (xs ++ [x]) = pick N xs ++ map (++ [x]) (pick N xs ++ pick M xs)

pickall xs = (pick E xs, pick S xs, pick M xs, pick N xs)
pickall = foldl step ([[]], [], [], [])
step (ess, sss, mss, nss) x = (ess
                              , map (++ [x]) (sss +++ ess)
                              , mss ++ sss
                              , nss ++ map (++ [x]) (nss ++ mss)
                              )

--mnss = maximum . map sum . fourth . pickall
--mnss = fourth . tuple (maximum . map sum) . pickall
mnss xs          = fourth (foldl h (start (take 3 xs)) (drop 3 xs))
start [x, y, z]  = 0, max [x + y + z, y + z, z], max [x, x + y, y], x + z)
h (e, s, m, n) x = (e, (s max e) + x, m max s, n max ((n max m) + x))

tuple f (w, x, y, z) = (f w, f x, f y, f z)
fourth xs = xs !! 3


