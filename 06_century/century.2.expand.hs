--candidates :: Data -> [Candidate]
--value      :: Candidate -> Value
--good       :: Value -> Bool

-- solutions :: Data -> [Candidate]
-- solutions :: filter (good . value) . candidates

--candidates = foldr extend []

type Expression = [Term]
type Term       = [Factor]
type Factor     = [Digit]
type Digit      = Int

valExpr :: Expression -> Int
valExpr = sum . map valTerm
valTerm :: Term -> Int
valTerm = product . map valFact
valFact :: Factor -> Int
valFact = foldl1 (\ n d -> 10 * n + d)

--good   :: Int -> Bool
--good v = (v == 100)

--expressions = foldr extend []
--extend                   :: Digit -> [Expression] -> [Expression]
--extend x []              = [[[[x]]]]
--extend x es              = concatMap (glue x) es
--glue                     :: Digit -> Expression -> [Expression]
--glue x ((xs: xss): xsss) = [((x: xs) : xss): xsss,
--                             ([x]: xs: xss): xsss,
--                             [[x]]: (xs: xss): xsss]
--xs: Factor = [Digit]
--xss: Term = [Factor]
--xsss: Expression = [Term]

--solutions = filter (good . valExpr) . expressions


--another definition of solutions
value ((xs: xss): xsss) = (10^n, valFact xs, valTerm xss, valExpr xsss)
                          where n = length xs
modify x (k, f, t, e) =
  [(10 * k, k * x + f, t, e), (10, x, f * t, e), (10, x, 1, f * t + e)]

good c (k, f, t, e) = (f * t + e == c)
ok c (k, f, t, e)   = (f * t + e <= c)

solutions c = map fst . filter (good c . snd) . foldr (expand c) []
--expand x c  = filter (ok c . snd) . zip . cross (extend x, modify x) . unzip
--cross (f, g) (x, y) = (f x, g y)

expand c x [] = [([[[x]]], (10, x, 1, 0))]
expand c x evs = concatMap (filter (ok c . snd) . glue x) evs
glue x ((xs: xss): xsss, (k, f, t, e)) =
  [(((x: xs): xss): xsss, (10 * k, k * x + f, t, e)),
   (([x]: xs: xss): xsss, (10, x, f * t, e)),
   ([[x]]: (xs: xss): xsss, (10, x, 1, f * t + e))]

