import qualified Data.List as Li

maxtail :: Ord a => [a] -> [a]
maxtail = maximum . Li.tails

--maxtail []          = []
--maxtail (xs ++ [x]) = op (maxtail xs) x
--maxtail = foldl op []

--op ys x = maximum [zs ++ [x] | zs <- tails ys, zs `isPrefixOf` ys] (14.2)
--does not work
--(xs ++ ys) after xs = ys
op ys x | null ys                  = [x]
        | head (ys after zs) >= x  =  ys ++ [x]
        | otherwise                =  op zs x
          where zs = border ys

--borders xs = [ys | ys <- tails xs, ys `isPrefixOf` xs]
borders [] = [[]]
borders xs = xs: borders (border xs)

border [x] = []
border (ys ++ [x]) | head (ys after zs) < x   =  border (zs ++ [x])
                   | head (ys after zs) == x  =  zs ++ [x]
                   | head (ys after zs) > x   =  []
                     where z = border ys

-- *Main> maxtail "introduction"
-- "uction"
-- *Main> borders "mammam"
-- ["mammam","mam","m",""]
