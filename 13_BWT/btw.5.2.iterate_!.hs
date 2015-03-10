import Data.List
import qualified Data.Array as Arr

transform    :: Ord a => [a] -> ([a], Int)
transform xs = (map last xss, position xs xss)
               where xss = sort (rots xs)

position xs xss = length (takeWhile (/= xs) xss)

rots    :: [a] -> [[a]]
rots xs = take (length xs) (iterate lrot xs)
          where lrot (x: xs) = xs ++ [x]


untransform :: Ord a => ([a], Int) -> [a]
--untransform . transform = id
--untransform (ys, k) = (recreate n ys) !! k
--  where n = length ys
--untransform (ys, k) = take (length ys) (tail (map (ys!!) (iterate (p!!) k)))
--  where
--    p = map snd (sort (zip ys [0 .. n - 1]))
--    n = length ys
untransform (ys, k) = take n (tail (map (ya Arr.!) (iterate (pa Arr.!) k)))
  where n  = length ys
        ya = Arr.listArray (0, n - 1) ys
        pa = Arr.listArray (0, n - 1) (map snd (sort (zip ys [0..])))


--recreate :: Ord a => [a] -> [[a]]
--recreate . map last . sort . rots = sort . rots
--recreate 0       = map (const [])
--recreate (j + 1) = hdsort . consCol . fork (id, recreate j)
--recreate (j + 1) = consCol . fork (apply p, apply p . recreate j)
--recreate j = tp . take j . tail . iterate (apply p)

-- does not work
--tp . take 0       = map (const [])
--tp . take (j + 1) = consCol . fork (head, tp . take j . tail)
--apply p . tp      = tp . map (apply p)

--fork (f, g) x = (f x, g x)

--rrot    :: [a] -> [a]
--rrot xs = [last xs] ++ init xs

--hdsort :: Ord a => [[a]] -> [[a]]
--hdsort = sortBy cmp where cmp (x: xs) (y: ys) = compare x y

-- does not work
--p = map snd (sort (zip ys [0 .. n - 1]))
--apply p xs = [xs Arr.!! i | i <- p]

--consCol           :: Ord a => ([a], [[a]]) -> [[a]]
--consCol (xs, xss) = zipWith (:) xs xss

-- *Main> transform "this, that or the other"
-- ("te,rshhhtttth  oeia  or",22)
-- *Main> untransform ("te,rshhhtttth  oeia  or",22)
-- "this, that or the other"

