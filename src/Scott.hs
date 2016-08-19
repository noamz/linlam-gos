-- code related to Dana Scott (1976), "Data Types as Lattices"

module Scott where

import Data.List

-- fromPair/toPair defines a bijection from pairs of natural numbers to natural numbers

fromPair :: (Int,Int) -> Int
fromPair (n,m) = (n+m)*(n+m+1) `div` 2 + m

toPair :: Int -> (Int,Int)
toPair i =
  let x = floor $ ((-1 + sqrt (fromInteger $ toInteger $ 1 + 8*i)) / 2.0) in
  let corner = fromPair (x,0) in
  (x - (i-corner),i-corner)

-- [toPair i | i <- [0..14]] == [(0,0),(1,0),(0,1),(2,0),(1,1),(0,2),(3,0),(2,1),(1,2),(0,3),(4,0),(3,1),(2,2),(1,3),(0,4)]
