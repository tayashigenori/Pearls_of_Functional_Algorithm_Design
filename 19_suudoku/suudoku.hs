-- 19.2. 仕様
type Matrix a = [Row a]
type Row a = [a]

type Grid = Matrix Digit
type Digit = Char

digits = [1..9]
blank = (== 0)

--solve version 1
solve1 = filter valid . expand . choices

choices :: Grid -> Matrix Choices
expand  :: Matrix Choices -> [Grid]
valid   :: Grid -> Bool

--choices
type Choices = [Digit]

choices :: Grid -> Matrix Choices
choices = map (map choices)
choices d = if blank d then digits else [d]

--expand
expand :: Matrix Choices -> [Grid]
expand = cp . map cp

cp           :: [[a]] -> [[a]]
cp []        = [[]]
cp (xs: xss) = [x:ys | x <- xs, ys <- cp xs]

--valid
valid :: Grid -> Bool
valid g = all nodups (rows g)
       && all nodups (cols g)
	   && all nodups (boxs g)

nodups    :: Eq a => [a] -> Bool
nodups [] = True
nodups (x: xs) = all (/= x) xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs: xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group

group    :: [a] -> [[a]]
group [] = []
group xs = take 3 xs: group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat

--rows . rows = id
--cols . cols = id
--boxs . boxs = id

--map rows . expand = expand . rows --(19.1)
--map cols . expand = expand . cols --(19.2)
--map boxs . expand = expand . boxs --(19.3)

-- 19.3. 選択肢行列の枝刈り
prune:: Matrix Choices -> Matrix Choices

pruneRow     :: Row Choices -> Row Choices
pruneRow row = map (remove fixed) row
               where fixed = [d | [d] <- row]

remove xs ds = if single ds then ds else ds \\ xs
--filter nodups . cp = filter nodups . cp . pruneRow --(19.4)

--if f.f = id then
--filter (p . f) = map f . filter p . map f --(19.5)

--filter (all p) . cp = cp . map (filter p) --(19.6)

--filter (all nodups . boxs) . expand
--    = filter (all nodups . boxs) . expand . pruneBy boxs
pruneBy f = f . map pruneRow . f

filter valid . expand = filter valid . expand . prune
prune = pruneBy boxs . pruneBy cols . pruneBy rows

-- solve version 2
solve2 = filter valid . expand . prune . choices

-- 19.4. 単一マス拡張
expand = concat . map expand . expand1 --(19.7)

expand1      :: Matrix Choices -> [Matrix Choices]
expand1 rows = [rows1
                ++ [row1 ++ [c]: row2]
				++ rows2 | c <- cs]
  where (rows1, row: rows2) = break (any smallest) rows
        (row1,  cs:  row2)  = break smallest row
		smallest cs         = length cs == n
		n                   = minimum (counts rows)
counts = filter (/= 1) . map length . concat

break p xs = (takeWhile (not . p) xs, dropWhile (not . p) xs)

complete = all (all single)
safe m = all ok (rows m) && all ok (cols m) && all ok (boxs m)
  where ok row = nodups [d | [d] <- row]

search = filter valid . expand . prune
--search = concat . map search . expand1 . prune

-- solve version 3
solve3 = search . choices
search m | not (safe m) = []
         | complete m'  = [map (map head) m']
		 | otherwise    = concat (map search (expand1 m'))
		   where m' = prune m

