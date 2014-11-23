--candidates :: Data -> [Candidate]
--value      :: Candidate -> Value
--good       :: Value -> Bool

-- solutions :: Data -> [Candidate]
-- solutions :: filter (good . value) . candidates

candidates = foldr extend []

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

good   :: Int -> Bool
good v = (v == 100)

expressions = foldr extend []
extend                   :: Digit -> [Expression] -> [Expression]
extend x []              = [[[[x]]]]
extend x es              = concatMap (glue x) es
glue                     :: Digit -> Expression -> [Expression]
glue x ((xs: xss): xsss) = [((x: xs) : xss): xsss,
                            ([x]: xs: xss): xsss,
                             [[x]]: (xs: xss): xsss]
--xs: Factor = [Digit]
--xss: Term = [Factor]
--xsss: Expression = [Term]

solutions = filter (good . valExpr) . expressions
