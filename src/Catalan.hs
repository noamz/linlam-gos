{-# LANGUAGE DeriveDataTypeable #-}

module Catalan where

import Data.Maybe
import Data.Typeable

data Tree = L | B Tree Tree
  deriving (Show,Eq)

data Arc = U Int | D Int
  deriving (Show,Eq,Ord,Typeable)
type Arcs = [Arc]

nodes :: Tree -> Int
nodes L = 0
nodes (B t1 t2) = 1 + nodes t1 + nodes t2

leaves :: Tree -> Int
leaves L = 1
leaves (B t1 t2) = leaves t1 + leaves t2

binary_trees :: Int -> [Tree]
binary_trees 0 = [L]
binary_trees n = [B t1 t2 | i <- [0..n-1], t1 <- binary_trees i, t2 <- binary_trees (n-1-i)]

arcs2tree_cps :: Int -> Arcs -> (Arcs -> Tree -> r) -> r
arcs2tree_cps x (D y:w) k = if x == y then k w L else error "not a planar arc diagram"
arcs2tree_cps x (U y:w) k = arcs2tree_cps y w $ \w' t1 -> arcs2tree_cps x w' $ \w'' t2 -> k w'' (B t1 t2)
arcs2tree_cps x [] k = error "not a closed arc diagram"

arcs2tree :: Arcs -> Tree
arcs2tree [] = L
arcs2tree (U x:xs) = arcs2tree_cps x xs (\w' t -> B t (arcs2tree w'))
arcs2tree (D x:xs) = error "not a closed arc diagram"

tree2arcs_st :: Tree -> Int -> (Int,Arcs)
tree2arcs_st L n = (n,[])
tree2arcs_st (B t1 t2) n =
  let (n',w1) = tree2arcs_st t1 (n+1) in
  let (n'',w2) = tree2arcs_st t2 n' in
  (n'',U n : w1 ++ D n : w2)

tree2arcs :: Tree -> Arcs
tree2arcs t = let (n,w) = tree2arcs_st t 0 in w

normalizeArcs :: Arcs -> Arcs
normalizeArcs w =
  scan 0 [] w
  where
    scan :: Int -> [(Int,Int)] -> Arcs -> Arcs
    scan n sigma [] = []
    scan n sigma (U x:w) = U n:scan (n+1) ((x,n):sigma) w
    scan n sigma (D x:w) = D (fromJust $ lookup x sigma):scan n sigma w

arcs2dow :: Arcs -> [Int]
arcs2dow [] = []
arcs2dow (U x:w) = x:arcs2dow w
arcs2dow (D x:w) = x:arcs2dow w

dow2arcs :: [Int] -> Arcs
dow2arcs w = marked w []
  where
    marked :: [Int] -> [Int] -> Arcs
    marked [] seen = []
    marked (x:xs) seen = if elem x seen then D x:marked xs seen else U x:marked xs (x:seen)

arcs2signs :: Arcs -> [Bool]
arcs2signs [] = []
arcs2signs (U _:w) = False:arcs2signs w
arcs2signs (D _:w) = True:arcs2signs w

-- coercions checking that a tree has a special form

rightleaf :: Tree -> Tree
rightleaf (B t L) = t
rightleaf _ = error "tree not of form (B t L)"

leftleaf :: Tree -> Tree
leftleaf (B L t) = t
leftleaf _ = error "tree not of form (B L t)"
