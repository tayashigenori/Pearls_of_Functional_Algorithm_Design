--allcp xs = map (llcp xs) (tails xs)

allcp xs = fst4 (until (done n) (step xs) ([n], 0, 0, 1))
           where n = length xs

done  n (as, i, p, k) = k == n
step xs (as, i, p, k)
    | k >= i + p = (snoc as a,         k, a, k + 1) -- j >= p �̏ꍇ
    | q /= r     = (snoc as (min q r), i, p, k + 1) -- j <= p �̏ꍇ����1
    | q == r     = (snoc as b,         k, b, k + 1) -- j <= p �̏ꍇ����2
      where q = as !! (k - i)
            r = p  -  (k - i)
            a = llcp xs (drop k xs)
            b = q + llcp (drop q xs) (drop (q + k) xs)

fst4 (a, b, c, d) = a
snoc xs x = xs ++ [x]

llcp xs []           = 0
llcp [] ys           = 0
llcp (x: xs) (y: ys) = if x == y then 1 + llcp xs ys else 0

-- *Main> allcp "abacabacab"
-- [10,0,1,0,6,0,1,0,2,0]
