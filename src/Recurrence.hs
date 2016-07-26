-- Recurrence equations for various families of lambda terms

module Recurrence where

import Formulas

-- lambda skeletons / planar terms

-- number of lambda skeletons, with k free leaves and n internal nodes
skel :: Integer -> Integer -> Integer
skel k n = i (k == 1 && n == 0) + sum [skel i m * skel (k-i) (n-1-m) | i <- [0..k], m <- [0..n-1]] + cond (n > 0) (skel (k+1) (n-1))

-- [skel 0 (1+2*n) | n <- [0..]] == [1,4,32,336,4096,...] == A002005
-- [skel 1 (2*n) | n <- [0..]] == [1,3,24,256,3168,...] == A??

-- number of B/R-colorings of lambda skeletons (= neutral/normal planar
-- lambda terms), with k free leaves and n s-nodes
sneu :: Integer -> Integer -> Integer
snf :: Integer -> Integer -> Integer
sneu k n = i (k == 1 && n == 0) + sum [sneu i m * snf (k-i) (n-m) | i <- [1..k], m <- [0..n-1]]
snf k n = cond (n > 0) (sneu k (n-1)) + cond (k < n) (snf (k+1) n)

-- [sneu 1 n | n <- [0..]] == [1,1,3,14,83,570,4318,...] == A220910
-- [snf 0 n | n <- [1..]] == [1,2,9,54,378,2916,24057,...] == A000168

-- Tutte's formula for the number of rooted maps with n edges (OEIS A000168)
tutte :: Integer -> Integer
tutte n = (2 * fact (2*n) * 3^n) `div` (fact n * fact (n+2))
-- tutte n == 3^n * catalan n * 2/(n+2) == 3^n * catalan (n+1) `div` (2*n+1)

-- number of linear lambda terms, with k free vars and n internal nodes
linear :: Integer -> Integer -> Integer
linear k n = i (k == 1 && n == 0) + sum [linear i m * linear (k-i) (n-1-m) * binom i k | i <- [0..k], m <- [0..n-1]] + cond (n > 0) (linear (k+1) (n-1))

-- [linear 0 (1+2*n) | n <- [0..]] == [1,5,60,1105,27120,...] == A062980
-- [linear 1 (2*n) | n <- [0..]] == [1,4,50,960,24310,...] == A??

-- alternative (equivalent) computation of number of linear lambda terms
linear' :: Integer -> Integer -> Integer
linear' k n = i (k == 1 && n == 0) + sum [linear' i m * linear' (k-i) (n-1-m) | i <- [0..k], m <- [0..n-1]] + cond (n > 0) ((k+1) * linear (k+1) (n-1))

-- number of normal/neutral linear terms, with k free leaves and n s-nodes
linneu :: Integer -> Integer -> Integer
linnf :: Integer -> Integer -> Integer
linneu k n = i (k == 1 && n == 0) + sum [linneu i m * linnf (k-i) (n-m) * binom i k | i <- [1..k], m <- [0..n-1]]
linnf k n = cond (n > 0) (linneu k (n-1)) + cond (k < n) (linnf (k+1) n)

-- [linneu 1 n | n <- [0..]] == [1,1,4,33,438,8150,195698,...] == A??
-- [linnf 0 n | n <- [1..]] == [1,3,26,367,7142,176766,...] == A??

-- number of normal/neutral linear terms, quotiented by the exchange axiom
elinneu :: Integer -> Integer -> Integer
elinnf :: Integer -> Integer -> Integer
elinneu k n = i (k == 1 && n == 0) + sum [elinneu i m * elinnf (k-i) (n-m) * binom i k | i <- [1..k], m <- [0..n-1]]
elinnf k 0 = 0
elinnf k n = sum [(elinneu (k+p) (n-1)) `div` fact p | p <- [0..n-k]]

-- [elinnf 0 n | n <- [1..]] == [1,2,10,74,706,8162,110410,...] == A000698
-- [elinneu 1 n | n <- [0..]] == [1,1,3,15,105,945,10395,...] == A001147
-- [sum [elinnf k n `div` fact k | k <- [0..n]] | n <- [1..]] == [2,6,42,414,5058,72486,...] == A115974
-- [sum [elinneu k n `div` fact k | k <- [1..n+1]] | n <- [0..]] == A000698

-- [elinneu 2 n `div` 2 | n <- [0..]] == [0,1,5,32,260,2589,30669,...] == ??
-- [elinneu 3 n `div` 6 | n <- [0..]] == [0,0,2,22,234,2750,36500,...] == ??
-- note: the expression (elinneu k n `div` fact k) gives the coefficient of y^k z^n in
-- the generating function M(y,z) of Arquès & Beraud, counting rooted oriented maps
-- with k vertices and n edges.  By face/vertex duality, equivalently,
-- (elinneu k n `div` fact k) counts rooted oriented maps with k faces and n edges.

-- [elinnf 1 n | n <- [1..]] == [1,3,19,165,1769,22355,...] == ??

-- this seems to be the total number of vertices (and, equivalently,
-- faces) in all rooted maps with n+1 edges, cf. A005159, which begins
-- 1,3,18,135,1134,10206.  Note that among one-edge maps [cf. Jackson
-- & Visentin's atlas], there is 1 genus-one map with one vertex
-- (validating 19 = 18 + 1), and among two-edge maps, there are 10
-- genus-one maps with two vertices and 10 genus-one maps with one
-- vertex (validating 165 = 135 + 20 + 10).

-- Equivalently, this series counts the number of ways of picking a
-- rooted map together with a vertex contained in that map (possibly
-- equal to the root vertex).  By analogy, I would guess that

-- [elinnf 2 n | n <- [1..]] == [0,2,22,256,3320,48058,...] == ??

-- counts the total number of ways of picking an ordered pair of two
-- distinct vertices (faces) in a rooted map with n+1 edges.

-- count bridgeless neutral/normal planar terms
sneu_nb :: Integer -> Integer -> Integer
snf_nb :: Integer -> Integer -> Integer
sneu_nb k n = i (k == 1 && n == 0) + sum [sneu_nb i m * snf_nb (k-i) (n-m) | i <- [1..k-1], m <- [0..n-1]]
snf_nb k n = cond (n > 0) (sneu_nb k (n-1)) + cond (k < n) (snf_nb (k+1) n)

-- [snf_nb 0 n | n <- [1..]] == [1,1,3,13,68,399,2530,16965,118668,...] == A000260

-- count bridgeless linear lambda terms
linear_nb k n = i (k == 1 && n == 0) + sum [linear_nb i m * linear_nb (k-i) (n-1-m) * binom i k | i <- [1..k-1], m <- [0..n-1]] + cond (n > 0) (linear_nb (k+1) (n-1))

-- [linear_nb 0 (1+2*n) | n <- [0..]] == [1,2,20,352,8624,266784,...] == A267827

-- count bridgeless planar lambda terms
planar_nb k n = i (k == 1 && n == 0) + sum [planar_nb i m * planar_nb (k-i) (n-1-m) | i <- [1..k-1], m <- [0..n-1]] + cond (n > 0) (planar_nb (k+1) (n-1))

-- [planar_nb 0 (1+2*n) | n <- [0..]] == [1,1,4,24,176,1456,13056,...] == A000309

-- count bridgeless NLTs
linneu_nb k n = i (k == 1 && n == 0) + sum [linneu_nb i m * linnf_nb (k-i) (n-m) * binom i k | i <- [1..k-1], m <- [0..n-1]]
linnf_nb k n = cond (n > 0) (linneu_nb k (n-1)) + cond (k < n) (linnf_nb (k+1) n)

-- [linnf_nb 0 n | n <- [1..]] == [1,2,16,224,4408,110744,...] == ??

-- count bridgeless NLTs modulo exchange
elinneu_nb k n = i (k == 1 && n == 0) + sum [elinneu_nb i m * elinnf_nb (k-i) (n-m) * binom i k | i <- [1..k-1], m <- [0..n-1]]
elinnf_nb k 0 = 0
elinnf_nb k n = sum [(elinneu_nb (k+p) (n-1)) `div` fact p | p <- [0..n-k]]

-- [elinnf_nb 0 n | n <- [1..]] == [1,1,4,27,248,2830,38232,...] == A000699
-- [elinneu_nb 2 n `div` 2 | n <- [1..]] == [1,2,10,76,754,9100,128436,...] == ??

