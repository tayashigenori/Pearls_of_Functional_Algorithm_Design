import Data.List

transform    :: Ord a => [a] -> ([a], Int)
transform xs = (map last xss, position xs xss)
               where xss = sort (rots xs)

position xs xss = length (takeWhile (/= xs) xss)

rots    :: [a] -> [[a]]
rots xs = take (length xs) (iterate lrot xs)
          where lrot (x: xs) = xs ++ [x]


--untransform :: Ord a => ([a], Int) -> [a]
--untransform . transform = id
--untransform (ys, k) = (recreate ys) !! k

--recreate :: Ord a => [a] -> [[a]]
--recreate . map last . sort . rots = sort . rots

-- *Main> transform "this, that or the other"
-- ("te,rshhhtttth  oeia  or",22)
