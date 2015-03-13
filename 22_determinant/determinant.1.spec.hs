det       :: [[Integer]] -> Integer
det [[x]] = x
det xss   = foldr1 (-) (zipWith (*) col1 (map det (minors cols)))
            where col1 = map head xss
                  cols = map tail xss

minors         :: [a] -> [[a]]
minors []      = []
minors (x: xs) = xs: map (x:) (minors xs)

-- *Main> minors "abcd"
-- ["bcd","acd","abd","abc"]
-- *Main> det [[0,1,2], [1,2,3], [2,3,4]]
-- 0
-- *Main> det [[1,9,8], [2,9,1], [3,5,7]]
-- -177
