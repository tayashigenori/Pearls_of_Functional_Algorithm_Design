import qualified Data.Array as Arr

--tails    :: [a] -> [[a]]
--tails [] = []
--tails xs = xs: tails (tail xs)

rank    :: Ord a => [a] -> [Int]
rank xs = map (\x -> length (filter (< x) xs)) xs

ranktails :: Ord a => [a] -> [Int]
--ranktails = rank . tails
ranktails   = applyUntil isperm rerankings . rank
rerankings  = map rerank (iterate (*2) 1)
rerank k rs = rs << shiftBy k rs

xs << ys = rank (zip xs ys)

applyUntil             :: (a -> Bool) -> [a -> a] -> a -> a
applyUntil p (f: fs) x = if p x then x else applyUntil p fs (f x)

isperm    :: [Int] -> Bool
isperm is = and (Arr.elems
            (Arr.accumArray (||) False (0, n - 1) (zip is (repeat True))))
            where n = length is

shiftBy k rs = map (+ k) (drop k rs) ++ [k - 1, k - 2 .. 0]
