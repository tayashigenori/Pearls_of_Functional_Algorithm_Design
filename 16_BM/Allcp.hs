module Allcp where

--tmp

allcp xs = [llcp xs (drop k xs) | k <- [0 .. length xs - 1]]
allcp' xs = tail (allcp xs) ++ [0]

llcp xs  []         = 0
llcp [] ys          = 0
llcp (x: xs) (y:ys) = if x == y then 1 + llcp xs ys else 0

