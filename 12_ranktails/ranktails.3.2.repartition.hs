import qualified Data.Array as Arr

--tails    :: [a] -> [[a]]
--tails [] = []
--tails xs = xs: tails (tail xs)

--rank    :: Ord a => [a] -> [Int]
--rank xs = map (\x -> length (filter (< x) xs)) xs
--rank = resort . concat . label . psort . zip [0 ..]

psort                  :: Ord b => [(a, b)] -> [[a]]
psort xys              = pass xys []
pass []            xss = xss
pass ((x, y): xys) xss = step xys [] [x] [] xss
  where
    step [] as bs cs xss = pass as (bs: pass cs xss)
    step (e@(x, y'): xys) as bs cs xss
      | y' <  y  =  step xys (e: as) bs cs xss
      | y' == y  =  step xys as (x: bs) cs xss
      | y' >  y  =  step xys as bs (e: cs) xss

-- ranktails
ranktails     :: Ord a => [a] -> [Int]
--ranktails   = rank . tails
--ranktails   = applyUntil isperm rerankings . rank
--rerankings  = map rerank (iterate (*2) 1)
--rerank k rs = rs << shiftBy k rs
--ranktails   = resort . concat . label . applyUntil (all single) repartitions . partition
ranktails xs  = (resort n . concat . label
                 . applyUntil (all single) (repartitions n)
                 . psort . zip [0 ..]) xs
                 where n = length xs
--partition             :: Ord a => [a] -> [[Int]]
--partition             = psort . zip [0..]
--partition (zip xs ys) = concatMap (psort . map (install ys)) (partition xs)
--resort     :: [(Int, Int)] -> [Int]
--resort ijs = Arr.elems (Arr.array (0, length ijs - 1) ijs)
resort n     = Arr.elems . Arr.array (0, n - 1)
label        :: [[a]] -> [[(a, Int)]]
--label xss  = zipWith tag xss (scanl (+) 0 (map length xss))
--tag xs k   = [(x, k) | x <- xs]
label iss    = zipWith tag iss (scanl (+) 0 (map length iss))
tag is j     = [(i, j) | i <- is]
--repartitions      = map repartition (iterate (*2) 1)
repartitions n      = map (repartition n) (iterate (*2) 1)
repartition n k iss = concatMap (psort . map install) iss
--repartition k iss = partition (zip rs (shiftBy k rs))
--                    where rs = resort (concat (label iss))
-- improvement 1
--repartition k iss = concatMap (psort . map (install rs)) iss
--                    where rs = shiftBy k (resort (concat (label iss)))
  where install i = (i, if j < n then k + a Arr.! j else n - i - 1)
                    where j = i + k
        a         = Arr.array (0, n - 1) (concat (label iss))

--xs << ys = rank (zip xs ys)

applyUntil             :: (a -> Bool) -> [a -> a] -> a -> a
applyUntil p (f: fs) x = if p x then x else applyUntil p fs (f x)

--isperm    :: [Int] -> Bool
--isperm is = and (Arr.elems
--            (Arr.accumArray (||) False (0, n - 1) (zip is (repeat True))))
--            where n = length is

single xs = (length xs == 1)
shiftBy k rs = map (+ k) (drop k rs) ++ [k - 1, k - 2 .. 0]
