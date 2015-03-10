import qualified Data.List as Li

maxtail :: Ord a => [a] -> [a]
maxtail = maximum . Li.tails

--maxtail []          = []
--maxtail (xs ++ [x]) = op (maxtail xs) x
--maxtail = foldl op []

--op ys x = maximum [zs ++ [x] | zs <- tails ys, zs `isPrefixOf` ys]

-- *Main> maxtail "introduction"
-- "uction"
