-- code related to Loday's "Arithmetree"

module Loday where

import Data.List
import Bijections
import Catalan
import Tamari
import Permutohedra

import qualified Lambda as Lam

lsumv :: Tree -> Tree -> [Tree]
rsumv :: Tree -> Tree -> [Tree]
sumv :: Tree -> Tree -> [Tree]

lsumv x L = [x]
lsumv (B xL xR) y = [B xL z | z <- sumv xR y]
lsumv L y = [] -- [y]
rsumv L y = [y]
rsumv x (B yL yR) = [B z yR | z <- sumv x yL]
rsumv x L = [] -- [x]
sumv x y = union (lsumv x y) (rsumv x y)

lsumt :: [Tree] -> [Tree] -> [Tree]
rsumt :: [Tree] -> [Tree] -> [Tree]
sumt :: [Tree] -> [Tree] -> [Tree]

lsumt xs ys = foldr union [] [lsumv x y | x <- xs, y <- ys]
rsumt xs ys = foldr union [] [rsumv x y | x <- xs, y <- ys]
sumt xs ys = foldr union [] [sumv x y | x <- xs, y <- ys]

tol (Lam.TVar _) = [B L L]
tol (Lam.TFn a b) = lsumt (tor a) (tol b) 
tor (Lam.TVar _) = [B L L]
tor (Lam.TFn a b) = rsumt (tor b) (tol a)

conj30 :: Int -> [Int]
conj30 n =
  let ts = Lam.allcNPTnb True (n+1) in
  let tps = map Lam.synthClosedNormal ts in
  map (\t -> length (tol t)) tps

toVec :: Tree -> [Int]
toVec L = []
toVec (B t1 t2) =
  let w1 = toVec t1 in
  let w2 = toVec t2 in
  let i = length w1 in
  let j = length w2 in
  let n = i+j in
  w1 ++ [n+1] ++ map (+i) w2

toPerm :: Tree -> Perm
toPerm t = zip [1..nodes t] (toVec t)

-- from Aguiar and Livernet, "The associative operad and the weak order on the symmetric groups"
fromPerm :: Perm -> Tree
fromPerm [] = L
fromPerm p =
  let n = length (dom p) in
  let j = act (inv p) n in
  let w = map (act p) [1..n] in
  let (wL,_:wR) = splitAt (j-1) w in
  B (fromPerm $ stdToPerm $ stdize wL) (fromPerm $ stdToPerm $ stdize wR)

-- [length $ [(t1,t2) | t1 <- binary_trees n, t2 <- binary_trees n, perm_le (toPerm t1) (toPerm t2)] | n <- [0..]] == [1,1,3,13,68,399,2530,...] == A000260

-- [length $ [(w,t) | w <- permute [1..n], t <- binary_trees n, perm_le w (toPerm t)] | n <- [0..]] == [1,1,3,14,85,621,5236,...] == A088716?

-- [length $ [(t,w) | w <- permute [1..n], t <- binary_trees n, perm_le (toPerm t) w] | n <- [0..]] == [1,1,3,15,105,945,10395,...] == A001147?

-- [length $ [(w,t) | w <- permute [1..n], t <- binary_trees n, tamari_seq [] (fromPerm w) t] | n <- [0..]] == [1,1,3,15,105,945,10395,...] == A001147?
