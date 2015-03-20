countdown4   :: Int -> [Int] -> (Expr, Value)
--countdown1 n = nearest n . concatMap mkExprs . subseqs
countdown4 n = nearest n . extract . memoise . subseqs

memoise        :: [[Int]] -> Memo
memoise        = foldl insert empty
insert memo xs = store xs (mkExprs memo xs) memo

data Trie a = Node a [(Int, Trie a)]
type Memo = Trie [(Expr, Value)]

empty :: Memo
fetch                       :: Memo -> [Int] -> [(Expr, Value)]
fetch (Node es xms) []      = es
fetch (Node es xms) (x: xs) = fetch (follow x xms) xs
follow                      :: Int -> [(Int, Memo)] -> Memo
follow x xms                = head [m | (x', m) <- xms, x == x']

store :: [Int] -> [(Expr, Value)] -> Memo -> Memo
store [x] es (Node fs xms) = Node fs ((x, Node es []) : xms)
store (x:xs) es (Node fs xms)
    = Node fs (yms ++ (x, store xs es m) : zms)
      where (yms, (z, m) : zms) = break (equals x) xms
            equals x (z, m)     = (x == z)

extract               :: Memo -> [(Expr, Value)]
extract (Node es xms) = es ++ concatMap (extract . snd) xms

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

--legal           :: Op -> Value -> Value -> Bool
--legal Add v1 v2 = True
--legal Sub v1 v2 = (v2 < v1)
--legal Mul v1 v2 = True
--legal Div v1 v2 = (v1 `mod` v2 == 0)
--legal Add v1 v2 = (v1 <= v2)
--legal Sub v1 v2 = (v2 < v1)
--legal Mul v1 v2 = (1 < v1) && (v1 <= v2)
--legal Div v1 v2 = (1 < v2) && (v1 `mod` v2 == 0)

legal :: Op -> (Expr, Value) -> (Expr, Value) -> Bool
legal Add (e1, v1) (e2, v2)
  =       (v1 <= v2) && non Sub e1 && non Add e2 && non Sub e2
legal Sub (e1, v1) (e2, v2)
  =       (v2 < v1) && non Sub e1 && non Sub e2
legal Mul (e1, v1) (e2, v2)
  =       (1 < v1 && v1 <= v2) && non Div e1 && non Mul e2 && non Div e2
legal Div (e1, v1) (e2, v2)
  =       (1 < v2 && v1 `mod` v2 == 0) && non Div e1 && non Div e2

non                     :: Op -> Expr -> Bool
non op (Num x)          = True
non op1 (App op2 e1 e2) = (op1 /= op2)

--mkExprs :: [Int] -> [(Expr, Value)]
--mkExprs [x] = [(Num x, x)]
--mkExprs xs = [ev | (ys, zs) <- unmerges xs
--                 , ev1 <- mkExprs ys
--                 , ev2 <- mkExprs zs
--                 , ev <- combine ev1 ev2]
mkExprs          :: Memo -> [Int] -> [(Expr, Value)]
mkExprs memo [x] = [(Num x, x)]
mkExprs memo xs  = [ev | (ys, zs) <- unmerges xs
                       , ev1 <- fetch memo ys
                       , ev2 <- fetch memo zs
                       , ev <- combine ev1 ev2
                   ]

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
--combine (e1, v1) (e2, v2)
--   =  [(App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2]
--   ++ [(App op e2 e1, apply op v2 v1) | op <- ops, legal op v2 v1]
combine (e1, v1) (e2, v2)
  | v1 <  v2  =  comb1 (e1, v1) (e2, v2)
  | v1 == v2  =  comb2 (e1, v1) (e2, v2)
  | v1 >  v2  =  comb1 (e2, v2) (e1, v1)

--comb1 (e1, v1) (e2, v2)
--  = [(App Add e1 e2, v1 + v2), (App Sub e2 e1, v2 - v1)] ++
--    if 1 < v1
--    then [(App Mul e1 e2, v1 * v2)] ++ [(App Div e2 e1, q) | r == 0]
--    else []
--      where (q, r) = divMod v2 v1
comb1 (e1, v1) (e2, v2)
  = (if non Sub e1 && non Sub e2 then
     [(App Add e1 e2, v1 + v2) | non Add e2] ++
     [(App Sub e2 e1, v2 - v1)]
     else []) ++
    (if 1 < v1 && non Div e1 && non Div e2 then
     [(App Mul e1 e2, v1 * v2) | non Mul e2] ++
     [(App Div e2 e1, q) | r == 0]
     else [])
      where (q, r) = divMod v2 v1
--comb2 (e1, v1) (e2, v2)
--  = [(App Add e1 e2, v1 + v2)] ++
--    if 1 < v1
--    then [(App Mul e1 e2, v1 * v2)] ++ [(App Div e1 e2, 1)]
--    else []
comb2 (e1, v1) (e2, v2)
  = [(App Add e1 e2, v1 + v2) | non Sub e1, non Add e2, non Sub e2] ++
    (if 1 < v1 && non Div e1 && non Div e2 then
     [(App Mul e1 e2, v1 * v2) | non Mul e2] ++ [(App Div e1 e2, 1)]
    else [])

ops = [Add, Sub, Mul, Div]

nearest n ((e, v): evs) = if d == 0 then (e, v)
                          else search n d (e, v) evs
                          where d = abs n - v
search n d ev [] = ev
search n d ev ((e, v): evs) | d' == 0 = (e, v)
                            | d' <  d = search n d' (e, v) evs
                            | d' >= d = search n d ev evs
                              where d' = abs n - v

-- *Main> display (countdown4 831 [1,3,7,10,25,50])
-- 
-- <interactive>:36:1: Not in scope: ‘display’

