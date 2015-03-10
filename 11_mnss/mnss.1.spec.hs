{-# LANGUAGE NPlusKPatterns #-}

mnss :: [Int] -> Int
mnss = maximum . map sum . nonsegs

-- markings
markings    :: [a] -> [[(a, Bool)]]
markings xs = [zip xs bs | bs <- booleans (length xs)]

booleans 0 = [[]]
booleans (n + 1) = [b: bs | b <- [True, False], bs <- booleans n]

-- nonsegs
nonsegs :: [a] -> [[a]]
nonsegs = extract . filter nonseg . markings

extract :: [[(a, Bool)]] -> [[a]]
extract = map (map fst . filter snd)

-- nonseg
data State = E | S | M | N deriving Eq

--does not work
nonseg = (== N) . foldl step E . map snd

step E False = E
step E True  = S
step S False = M
step S True  = S
step M False = M
step M True  = N
step N False = N
step N True  = M
