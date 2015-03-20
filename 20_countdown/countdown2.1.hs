countdown2   :: Int -> [Int] -> (Expr, Value)
countdown2 n = nearest n . concatMap mkExprs . subseqs

subseqs [x]     = [[x]]
subseqs (x: xs) = xss ++ [x]: map (x:) xss
                  where xss = subseqs xs

data Expr  = Num Int | App Op Expr Expr
data Op    = Add | Sub | Mul | Div deriving (Eq)
type Value = Int

value                :: Expr -> Value
value (Num x)        = x
value (App op e1 e2) = apply op (value e1) (value e2)

apply          :: Op -> Int -> Int -> Int
apply Add x y  =  x + y
apply Sub x y  =  x - y
apply Mul x y  =  x * y
apply Div x y  =  x `div` y

legal           :: Op -> Value -> Value -> Bool
--legal Add v1 v2 = True
--legal Sub v1 v2 = (v2 < v1)
--legal Mul v1 v2 = True
--legal Div v1 v2 = (v1 `mod` v2 == 0)
legal Add v1 v2 = (v1 <= v2)
legal Sub v1 v2 = (v2 < v1)
legal Mul v1 v2 = (1 < v1) && (v1 <= v2)
legal Div v1 v2 = (1 < v2) && (v1 `mod` v2 == 0)

mkExprs     :: [Int] -> [(Expr, Value)]
mkExprs [x] = [(Num x, x)]
mkExprs xs  = [ev | (ys, zs) <- unmerges xs
                  , ev1 <- mkExprs ys
                  , ev2 <- mkExprs zs
                  , ev <- combine ev1 ev2]

unmerges         :: [a] -> [([a], [a])]
--unmerges [x, y]  = [([x], [y]), ([y], [x])]
--unmerges (x: xs) = [([x], xs), (xs, [x])]
--                 ++ concatMap (add x) (unmerges xs)
--                    where add x (ys, zs) = [(x: ys, zs), (ys, x: zs)]
unmerges [x, y]  = [([x], [y])]
unmerges (x: xs) = [([x], xs)] ++ concatMap (add x) (unmerges xs)
                    where add x (ys, zs) = [(x: ys, zs), (ys, x: zs)]

combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
--combine (e1, v1) (e2, v2)
--   = [(App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2]
combine (e1, v1) (e2, v2)
   =  [(App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2]
   ++ [(App op e2 e1, apply op v2 v1) | op <- ops, legal op v2 v1]

ops = [Add, Sub, Mul, Div]

nearest n ((e, v): evs) = if d == 0 then (e, v)
                          else search n d (e, v) evs
                          where d = abs n - v
search n d ev [] = ev
search n d ev ((e, v): evs) | d' == 0 = (e, v)
                            | d' <  d = search n d' (e, v) evs
                            | d' >= d = search n d ev evs
                              where d' = abs n - v

-- *Main> display (countdown2 831 [1,3,7,10,25,50])
-- 
-- <interactive>:36:1: Not in scope: ‘display’
-- *Main> length $ concatMap mkExprs $ subseqs [1,3,7,10,25,50]
-- 245644
