--cabal install queue
--import qualified Data.Queue as Q
-- did not work!!
import qualified Data.Array as Arr

--allcp xs = map (llcp xs) (tails xs)

allcp xs = extract (until done step (as, Q.empty, 0, 1))
  where
    extract (as, qs, h, k) = Q.elems as
    done    (as, qs, h, k) = (k == n)
    n                      = length xs
    as                     = Q.insert Q.empty n
    xa                     = Arr.listArray (0, n - 1) xs
    step (as, qs, h, k) | k >= h  =  (Q.insert as a, Q.insert as' a, k + a, k + 1)
                        | q /= r  =  (Q.insert as m, Q.insert qs' m, h,     k + 1)
                        | q == r  =  (Q.insert as b, Q.insert as' b, k + b, k + 1)
                          where as'      =  snd (Q.remove as)
                                (q, qs') =  Q.remove qs
                                r        =  h - k
                                m        = min q r
                                a        = llcp' 0 k
                                b        = q + llcp' q (q + k)

    llcp' j k  |  j == n || k == n  =  0
               |  xa Arr.! j == xa Arr.! k  =  1 + llcp' (j + 1) (k + 1)
               |  otherwise         =  0

-- Queue 型に対して可能な演算
-- insert
-- remove
-- empty
-- elems
