
matches :: Eq a => [a] -> [a] -> Int
matches ws = map length . filter (endswith ws) . inits

--matches "abcab" "ababcabcab" = [7, 10]

-- scan lemma
map (foldl op e) . inits = scanl op e

map f . filter p = map fst . filter snd . map (fork (f, p))

matches ws
   = map fst . filter snd . map (fork (length, endswith ws)) . inits

endswith ws = foldl op e

--the tupling law for foldl
fork (foldl op1 e1, foldl op2 e2) = foldl op (e1, e2)

fork (length, endswith ws) = foldl step (0, e)
step (n, x) y              = (n + 1, op x y)

matches ws = map fst . filter snd . scanl step (0, e)

endswith ws = p . foldl op e

map f . filter (p . g)
  = map fst . filter (p . snd) . map (fork (f, g))

matches ws = map fst . filter (p . snd) . scanl step (0, e)

