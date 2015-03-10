import Data.List

--endswith ws xs = ws `elem` tails xs

matches :: Eq a => [a] -> [a] -> [Int]
--matches ws = map fst . filter (p . snd) . scanl step (0, e)
--step (n, x) y = (n + 1, op x y)

--split ws xs = head [(us, ws ↓ us) | us <- tails xs, us `isPrefixOf` ws]
-- does not work!!
--split ws []          = ([], ws)
--split ws (xs ++ [x]) = op (split ws xs) x

split ws []  = ([], ws)
split ws [x] = op ([], ws) x
split ws xs  = op (split ws (init xs)) (last xs)

op (us, vs) x | [x] `isPrefixOf` vs = (us ++ [x], tail vs)
              | null us             = ([], ws)
              | otherwise           = op (split ws (tail us)) x
                where ws = us ++ vs

matches ws           = map fst . filter (null . snd . snd) . scanl step (0, ([], ws))
step (n, (us, vs)) x = (n + 1, op (us, vs) x)


-- *Main> (scanl step (0, ([], "abcab"))) "ababcabcab"
-- [(0,("","abcab")),(1,("a","bcab")),(2,("ab","cab")),(3,("a","bcab")),(4,("ab","cab")),(5,("abc","ab")),(6,("abca","b")),(7,("abcab","")),(8,("abc","ab")),(9,("abca","b")),(10,("abcab",""))]
