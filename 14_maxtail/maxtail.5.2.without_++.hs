import qualified Data.List as Li

--maxtail :: Ord a => [a] -> [a]
--maxtail = maximum . Li.tails

--maxtail []          = []
--maxtail (xs ++ [x]) = op (maxtail xs) x
--maxtail = foldl op []

--op ys x = maximum [zs ++ [x] | zs <- tails ys, zs `isPrefixOf` ys] (14.2)
--does not work
--(xs ++ ys) after xs = ys
--op ys x | null ys                  = [x]
--        | head (ys after zs) >= x  =  ys ++ [x]
--        | otherwise                =  op zs x
--          where zs = border ys

--borders xs = [ys | ys <- tails xs, ys `isPrefixOf` xs]
--borders [] = [[]]
--borders xs = xs: borders (border xs)

--border [x] = []
--border (ys ++ [x]) | head (ys after zs) < x   =  border (zs ++ [x])
--                   | head (ys after zs) == x  =  zs ++ [x]
--                   | head (ys after zs) > x   =  []
--                     where z = border ys

--maxtail = uncurry (++) . cocktail
--maxtail = thd . cocktail
--thd xs = xs!! 2
--cocktail xs = if null xs then ([], [])
--              else (border (maxtail xs), maxtail xs after border (maxtail xs))
--cocktail = foldl op ([], [])
--cocktail = foldl op (0, 0, [], [])
--does not work
--op (zs, ws) x | null ws  =  ([], [x])
--              | w < x    =  cocktail (take r zs ++ [x])
--              | w == x   =  (zs ++ [x], tail ws ++ [x])
--              | w > x    =  ([], zs ++ ws ++ [x])
--                where w = head ws
--                      r = (length zs) `mod` (length ws)
--op (p, q, ys, ws) x
--  | q == 0     =  (0, 1, [x], [x])
--  | w < x      =  cocktail (drop (q - r) ws ++ [x])
--  | w == x     =  (p + 1,         q, ys ++ [x], tail ws ++ [x])
--  | otherwise  =  (0,     p + q + 1, ys ++ [x],      ys ++ [x])
--    where w = head ws
--          r = p `mod` q

maxtail []      = []
maxtail (x: xs) = step (0, 1, x: xs, x: xs, xs)
step (p, q, ys,    ws,    []) = ys
step (p, q, ys, w: ws, x: xs)
  | w < x   =  maxtail (drop (q - r) (w: ws))
  | w == x  =  step (p + 1,         q, ys, ws, xs)
  | w > x   =  step (0,     p + q + 1, ys, ys, xs)
    where r = p `mod` q

-- *Main> maxtail "introduction"
-- "uction"
-- *Main> borders "mammam"
-- ["mammam","mam","m",""]
