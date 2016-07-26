module Formulas where

cond :: Bool -> Integer -> Integer
cond True n = n
cond False _ = 0

i :: Bool -> Integer
i b = cond b 1

fact 0 = 1
fact n = if n < 0 then -1 else n * fact (n-1)

-- binom k n = if k>n then 0 else fact n `div` (fact k * fact (n-k))
binom 0 n = 1
binom k 0 = 0
binom k n = binom (k-1) (n-1) + binom k (n-1)

catalan n = binom n (2*n) `div` (n+1)

dfact 0 = 1
dfact 1 = 0
dfact n = (n-1) * dfact (n-2)
