-- counting intervals in the permutohedron

module Permutohedra where

import Data.List

import Formulas
import Bijections

inversions :: Perm -> [(Int,Int)]
inversions pi = [(i,j) | i <- dom pi, j <- dom pi \\ [i], i < j, act (inv pi) i > act (inv pi) j]

perm_le :: Perm -> Perm -> Bool
perm_le pi1 pi2 = all (\(i,j) -> elem (i,j) (inversions pi2)) (inversions pi1)

perm_lattice :: Int -> [(Perm,Perm)]
perm_lattice n = do
  pi1 <- permute [1..n]
  pi2 <- permute [1..n]
  if perm_le pi1 pi2 then return (pi1,pi2) else []

-- [length $ perm_lattice n | n <- [0..]] == [1,1,3,17,151,1899,...]

a007767 :: Int -> [(Perm,Perm)]
a007767 n =
  [(pi1,pi2) | pi1 <- permute [1..n], pi2 <- permute [1..n], not $ any (\i -> any (\j -> i < j && (act pi1 i) < (act pi1 j) && (act pi2 j) < (act pi2 i)) [1..n]) [1..n]]

