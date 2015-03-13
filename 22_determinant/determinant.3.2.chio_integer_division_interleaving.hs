
det       :: [[Integer]] -> Integer
--det [[x]] = x
--det xss   = foldr1 (-) (zipWith (*) col1 (map det (minors cols)))
--            where col1 = map head xss
--                  cols = map tail xss
det = det' 1

--det       :: [[Ratio Integer]] -> Ratio Integer
--det [[x]] = x
--det xss   =
--  case break ((/= 0) . head) xss of
--    (yss, [])      -> 0
--    (yss, zs: zss) -> let x = head zs * det (reduce zs (yss ++ zss))
--                      in if even (length yss) then x else -x
--    (yss, zs: zss) -> let x = det (condense (zs: yss ++ zss))
--                          d = head zs ^ (length xss - 2)
--                          y = x `div` d
--                      in if even (length yss) then y else -y
det'         :: Integer -> [[Integer]] -> Integer
det' k [[x]] = x
det' k xss   =
  case break ((/= 0) . head) xss of
    (yss, [])      -> 0
    (yss, zs: zss) -> let x = det' (head zs) (cd k (zs: yss ++ zss))
                      in if even (length yss) then x else -x

--minors         :: [a] -> [[a]]
--minors []      = []
--minors (x: xs) = xs: map (x:) (minors xs)

--reduce xs yss           = map (reduce1 xs) yss
--reduce1 (x: xs) (y: ys) = zipWith (\a b -> b - d * a) xs ys
--                          where d = y / x

--condense = map (map det . pair . uncurry zip) . pair
--           where pair (x: xs)         = map ((,) x) xs
--                 det ((a, b), (c, d)) = a * d - b * c

cd k = map (map det . pair . uncurry zip) . pair
       where pair (x: xs)         = map ((,) x) xs
             det ((a, b), (c, d)) = (a * d - b * c) `div` k


-- *Main> det [[0,1,2], [1,2,3], [2,3,4]]
-- 0
-- *Main> det [[1,9,8], [2,9,1], [3,5,7]]
-- -177
