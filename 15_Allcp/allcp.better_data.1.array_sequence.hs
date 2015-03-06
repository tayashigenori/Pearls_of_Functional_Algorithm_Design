import qualified Data.Array as Arr
import qualified Data.Sequence as Seq

--allcp xs = map (llcp xs) (tails xs)

allcp xs = fst4 (until (done n) (step xs) ([n], 0, 0, 1))
           where n = length xs

fst4 (a, b, c, d) = a
done  n (as, i, p, k) = k == n
step xs (as, i, p, k)
    | k >= i + p = (snoc as a,         k, a, k + 1) -- j >= p の場合
    | q /= r     = (snoc as (min q r), i, p, k + 1) -- j <= p の場合その1
    | q == r     = (snoc as b,         k, b, k + 1) -- j <= p の場合その2
      where q = as !! (k - i)
            r = p  -  (k - i)
            a = llcp' 0 k
            b = q + llcp' q (q + k)
            xa = Arr.listArray (0, n - 1) xs
            n  = length xs
            llcp' j k | j == n || k == n  =  0
                      | xa Arr.! j == xa Arr.! k  =  1 + llcp' (j + 1) (k + 1)
                      | otherwise         =  0

-- Seq.snoc ??
snoc xs x = xs ++ [x]
-- Seq.!! ??

-- *Main> allcp "abacabacab"
-- [10,0,1,0,6,0,1,0,2,0]

-- Queue 型に対して可能な演算
-- insert
-- remove
-- empty
-- elems

