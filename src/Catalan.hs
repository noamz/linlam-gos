{-# LANGUAGE DeriveDataTypeable #-}

module Catalan where

import Data.List
import Data.Maybe
import Data.Typeable

import Bijections

data Tree = L | B Tree Tree
  deriving (Show,Eq,Ord,Typeable)

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

arcs2tree_cps :: Int -> Arcs -> (String -> r) -> (Arcs -> Tree -> r) -> r
arcs2tree_cps x (D y:w) fail k = if x == y then k w L else fail "not a planar arc diagram"
arcs2tree_cps x (U y:w) fail k = arcs2tree_cps y w fail $ \w' t1 -> arcs2tree_cps x w' fail $ \w'' t2 -> k w'' (B t1 t2)
arcs2tree_cps x [] fail k = fail "not a closed arc diagram"

arcs2tree :: Arcs -> Tree
arcs2tree [] = L
arcs2tree (U x:xs) = arcs2tree_cps x xs (\s -> error s) (\w' t -> B t (arcs2tree w'))
arcs2tree (D x:xs) = error "not a closed arc diagram"

isDyck :: Arcs -> Bool
isDyck [] = True
isDyck (U x:xs) = arcs2tree_cps x xs (\_ -> False) (\_ _ -> True)
isDyck (D _:_) = False

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

maparcs :: (Int -> Int) -> (Int -> Int) -> Arcs -> Arcs
maparcs f g [] = []
maparcs f g (U x:w) = U (f x) : maparcs f g w
maparcs f g (D x:w) = D (g x) : maparcs f g w

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

fliparcs :: Arcs -> Arcs
fliparcs [] = []
fliparcs (U x:w) = D x:fliparcs w
fliparcs (D x:w) = U x:fliparcs w

-- generate an arc diagram from an involution
inv2arcs :: [(Int,Int)] -> Arcs
inv2arcs f = map (\i -> let j = act f i in if i < j then U i else D j)
             (sort $ dom f)

-- generate an involution from an arc diagram
arcs2inv :: Arcs -> [(Int,Int)]
arcs2inv p =
  let n = length p in
  let act i = case p !! i of
        U x -> fromJust (findIndex (== D x) p)
        D x -> fromJust (findIndex (== U x) p) in
  [(i,act i) | i <- [0..n-1]]

-- dualize a tree by swapping left and right children

dualtree :: Tree -> Tree
dualtree L = L
dualtree (B t1 t2) = B (dualtree t2) (dualtree t1)

-- coercions checking that a tree has a special form

rightleaf :: Tree -> Tree
rightleaf (B t L) = t
rightleaf _ = error "tree not of form (B t L)"

leftleaf :: Tree -> Tree
leftleaf (B L t) = t
leftleaf _ = error "tree not of form (B L t)"

-- factor a tree along its left-branching spine

tree2spine :: Tree -> [Tree]
tree2spine L = []
tree2spine (B t1 t2) = t2 : tree2spine t1

spine2tree :: [Tree] -> Tree
spine2tree s = foldr (\y x -> B x y) L s

-- graft product: lgraft t1 t2 grafts t1 onto the leftmost leaf of t2

lgraft t1 t2 = foldr (\x y -> B y x) t1 (tree2spine t2)

-- rgraft t1 t2 grafts t2 onto the rightmost leaf of t1
rgraft t1 t2 = dualtree (lgraft (dualtree t2) (dualtree t1))
