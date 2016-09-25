-- counting intervals in the permutohedron

module Permutohedra where

import Data.List

import Formulas
import Bijections

inversions :: Perm -> [(Int,Int)]
inversions pi = [(i,j) | i <- dom pi, j <- dom pi \\ [i], i < j, act (inv pi) i > act (inv pi) j]

perm_le :: Perm -> Perm -> Bool
perm_le pi1 pi2 = all (\(i,j) -> elem (i,j) (inversions pi2)) (inversions pi1)

weak_bruhat = perm_le

perm_lattice :: Int -> [(Perm,Perm)]
perm_lattice n =
  [(pi1,pi2) | pi1 <- permute [1..n], pi2 <- permute [1..n], perm_le pi1 pi2]

-- [length $ perm_lattice n | n <- [0..]] == [1,1,3,17,151,1899,...]

a007767 :: Int -> [(Perm,Perm)]
a007767 n =
  [(pi1,pi2) | pi1 <- permute [1..n], pi2 <- permute [1..n], not $ any (\i -> any (\j -> i < j && (act pi1 i) < (act pi1 j) && (act pi2 j) < (act pi2 i)) [1..n]) [1..n]]

strong_bruhat :: Perm -> Perm -> Bool
strong_bruhat pi1 pi2 =
  let (n1,n2) = (length pi1,length pi2) in
  let (w1,w2) = (map (act pi1) (sort $ dom pi1),
                 map (act pi2) (sort $ dom pi2)) in
  (n1 == n2) &&
  flip all [1..n1-1] (\i ->
  all (uncurry (<)) (zip (sort (take i w1)) (sort (take i w2))))

-- [length $ [(pi1,pi2) | pi1 <- permute [1..n], pi2 <- permute [1..n], strong_bruhat pi1 pi2] | n <- [1..]] == [1,1,3,19,213,3781,...]

