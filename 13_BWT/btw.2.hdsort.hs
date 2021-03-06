{-# LANGUAGE NPlusKPatterns #-}
import Data.List

transform    :: Ord a => [a] -> ([a], Int)
transform xs = (map last xss, position xs xss)
               where xss = sort (rots xs)

position xs xss = length (takeWhile (/= xs) xss)

rots    :: [a] -> [[a]]
rots xs = take (length xs) (iterate lrot xs)
          where lrot (x: xs) = xs ++ [x]


untransform :: Ord a => ([a], Int) -> [a]
--untransform . transform = id
untransform (ys, k) = (recreate n ys) !! k
  where n = length ys

--recreate :: Ord a => [a] -> [[a]]
--recreate . map last . sort . rots = sort . rots
recreate 0       = map (const [])
recreate (j + 1) = hdsort . consCol . fork (id, recreate j)

fork (f, g) x = (f x, g x)

rrot    :: [a] -> [a]
rrot xs = [last xs] ++ init xs

hdsort :: Ord a => [[a]] -> [[a]]
hdsort = sortBy cmp where cmp (x: xs) (y: ys) = compare x y

consCol           :: Ord a => ([a], [[a]]) -> [[a]]
consCol (xs, xss) = zipWith (:) xs xss

-- *Main> transform "this, that or the other"
-- ("te,rshhhtttth  oeia  or",22)
-- *Main> untransform ("te,rshhhtttth  oeia  or",22)
-- "this, that or the other"

-- *Main> recreate 0 "eabcd"
-- ["","","","",""]
-- *Main> recreate 1 "eabcd"
-- ["a","b","c","d","e"]
-- *Main> consCol ("eabcd", ["a","b","c","d","e"])
-- ["ea","ab","bc","cd","de"]
-- *Main> recreate 2 "eabcd"
-- ["ab","bc","cd","de","ea"]
-- *Main> consCol ("eabcd", ["ab","bc","cd","de","ea"])
-- ["eab","abc","bcd","cde","dea"]
-- *Main> recreate 3 "eabcd"
-- ["abc","bcd","cde","dea","eab"]
-- *Main> recreate 5 "eabcd"
-- ["abcde","bcdea","cdeab","deabc","eabcd"]

