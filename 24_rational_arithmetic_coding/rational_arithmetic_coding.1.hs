import Data.Ratio

-- data

type Fraction = Ratio Integer
type Interval = (Fraction, Fraction)
type Symbol   = String
type Model    = Symbol -> Interval

-- narrowing
(|>)                 :: Interval -> Interval -> Interval
(l1, r1) |> (l2, r2) = (l1 + (r1 - l1) * l2, l1 + (r1 - l1) * r2)

(<|)        :: Fraction -> Interval -> Fraction
f <| (l, r) = (f - l) / (r - l)

-- model
interval :: Model -> Symbol -> Interval
symbol   :: Model -> Fraction -> Symbol

adapt :: Model -> Symbol -> Model

intervals           :: Model -> [Symbol] -> [Interval]
intervals m []      = []
intervals m (x: xs) = interval m x: intervals (adapt m x) xs

-- encode
encode   :: Model -> [Symbol] -> Fraction
encode m = pick . foldl (|>) (0, 1) . intervals m
