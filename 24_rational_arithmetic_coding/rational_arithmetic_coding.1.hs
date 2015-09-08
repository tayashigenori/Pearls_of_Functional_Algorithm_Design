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
--encode   :: Model -> [Symbol] -> Fraction
--encode m = pick . foldl (|>) (0, 1) . intervals m

-- decode
unfoldr     :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
                Just (a, b') -> a: unfoldr f b'
                Nothing      -> []
--decode         :: Model -> Fraction -> [Symbol]
--decode m f     = unfoldr step (m, (0, 1), f)
--step (m, i, f) = Just (x, (adapt m x,
--                           i |> interval m x,
--                           f))
--                 where x = symbol m (f <| i)

-- icremental encoding and decoding
type Bit = Int
toBits :: Interval -> [Bit]
toFrac :: [Bit] -> Fraction

--encode         :: Model -> [Symbol] -> [Bit]
--encode m       = toBits . foldl (|>) (0, 1) . intervals m
decode         :: Model -> [Bit] -> [Symbol]
devode m bs    = unfoldr step (m, (0, 1), toFrac bs)
step (m, i, f) = Just (x, (adapt m x,
                           i |> interval m x,
                           f))
                 where x = symbol m (f <| i)

-- streaming
stream f g s xs = unfoldr step (s, xs)
  where step (s, xs) = case f s of
                         Just (y, s') -> Just (y, (s', xs))
                         Nothing      -> case xs of
                                           x: xs' -> step (g s x, xs')
                                           []     -> Nothing
encode         :: Model -> [Symbol] -> [Bit]
encode m = stream bit (|>) (0, 1) . intervals m

bit (l, r) | r <= 1/2  = Just (0, (2 * l,     2 * r))
           | 1/2 <= l  = Just (1, (2 * l - 1, 2 * r - 1))
           | otherwise = Nothing
toBits = unfoldr bit
toFrac = foldr (\b f -> (b + f) / 2) (1/2)
-- or,
-- toFrac bs = foldr (\b f -> (b + f) / 2) 0 (bs ++ [1])
-- pick = toFrac . toBits
pick (l, r) | r <= 1/2  = pick (2 * l, 2 * r) /2
            | 1/2 <= l  = (1+ pick (2 * l - 1, 2 * r - 1)) / 2
            | otherwise = 1/2

