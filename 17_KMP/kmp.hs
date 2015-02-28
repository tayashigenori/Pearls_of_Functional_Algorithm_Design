
--endswith ws xs = ws `elem` tails xs

matches :: Eq a => [a] -> [a] -> [Int]
--matches ws = map fst . filter (p . snd) . scanl step (0, e)
--step (n, x) y = (n + 1, op x y)

--split ws xs = head [(us, ws ↓ us) | us <- tails xs, us `isPrefixOf` ws]
-- does not work!!
--split ws []          = ([], ws)
--split ws (xs ++ [x]) = op (split ws xs) x

op (ws vs) x | [x] `isPrefixOf` vs = (us ++ [x], tail vs)
             | null us             = ([], ws)
             | otherwise           = op (split ws (tail us)) x
               where ws = us ++ vs

matches ws           = map fst . filter (null . snd . snd) . scanl step (0, ([], ws))
step (n, (us, ws)) x = (n + 1, op (us, vs) x)

