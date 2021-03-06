
--endswith ws xs = ws `elem` tails xs

matches :: Eq a => [a] -> [a] -> [Int]
--matches ws = map fst . filter (p . snd) . scanl step (0, e)
--step (n, x) y = (n + 1, op x y)

--data Rep a = Null | Node a (Rep a) (Rep a)
data Rep a = Null | Node a (Rep a) (Rep a) deriving Show

matches ws = map fst . filter (ok . snd) . scanl step (0, root)
  where
    ok (Node vs l r)        = null vs
    step (n, t) x           = (n + 1, op t x)
    op Null x               = root
    op (Node []      l r) x = op l x
    op (Node (v: vs) l r) x = if v == x then r else op l x
    root                    = grep Null ws
    grep l []               = Node []      l Null
    grep l (v: vs)          = Node (v: vs) l (grep (op l v) vs)


window' ws = scanl step (0, root)
  where
    step (n, t) x           = (n + 1, op t x)
    op Null x               = root
    op (Node []      l r) x = op l x
    op (Node (v: vs) l r) x = if v == x then r else op l x
    root                    = grep Null ws
    grep l []               = Node []      l Null
    grep l (v: vs)          = Node (v: vs) l (grep (op l v) vs)
