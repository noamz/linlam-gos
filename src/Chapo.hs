module Chapo where

import Data.List
import Data.Maybe

import Bijections
import Lambda
import qualified Catalan as C
import qualified Tamari as T

-- extract a binary tree from a lambda term by looking at its
-- underlying applicative structure

apps2tree :: ULT -> C.Tree
apps2tree (L _ t) = apps2tree t
apps2tree (A t1 t2) = C.B (apps2tree t1) (apps2tree t2)
apps2tree (V _) = C.L

-- extract an arc diagram from a linear lambda term by
-- looking at its underlying binding structure (we can use either
-- the LR or the RL planarity convention to build the arc diagram)

lams2arcs :: Bool -> ULT -> C.Arcs
lams2arcs b (L x t) = C.U x : lams2arcs b t
lams2arcs b (A t1 t2) = if b then lams2arcs b t1 ++ lams2arcs b t2
                       else lams2arcs b t2 ++ lams2arcs b t1
lams2arcs b (V x) = [C.D x]

lams2arcsLR = lams2arcs True
lams2arcsRL = lams2arcs False

-- given a double-occurrence word representing its binding structure,
-- and a binary tree representing its applicative structure, reconstruct
-- a normal linear term

pluglamapp :: C.Arcs -> C.Tree -> Maybe ULT
pluglamapp (C.U x:w) c =
  if (C.D x) `elem` w then do
    u <- pluglamapp w c
    return $ L x u
  else Nothing
pluglamapp [C.D x] C.L = return $ V x
pluglamapp w (C.B c1 c2) = do
  let k = C.leaves c1
  let (w1,w2) = splitarcs k w
  u1 <- pluglamapp w1 c1
  u2 <- pluglamapp w2 c2
  return $ A u1 u2
  where
    splitarcs :: Int -> C.Arcs -> (C.Arcs,C.Arcs)
    splitarcs 0 w = ([],w)
    splitarcs n (C.U x:w) = let (w1,w2) = splitarcs n w in (C.U x:w1,w2)
    splitarcs n (C.D x:w) = let (w1,w2) = splitarcs (n-1) w in (C.D x:w1,w2)

-- extract an arc diagram from a normal linear lambda term by
-- looking at its principal type

type2arcsLR :: ULT -> C.Arcs
type2arcsLR t =
  C.dow2arcs $ (reverse . drop 2 . reverse) $ linearizeType $ synthClosedNormal t

type2arcsRL :: ULT -> C.Arcs
type2arcsRL t = C.dow2arcs $ linearizePos $ synthClosedNormal t

-- the number of normal planar indecomposable lambda terms of size n+1 is
-- equal to the number of intervals in the Tamari lattice T_n.

allnptiRL :: Int -> [ULT]
allnptiRL n = filter isIndecomposable $ allcNPT False (n+1)

allnptiLR :: Int -> [ULT]
allnptiLR n = filter isIndecomposable $ allcNPT True (n+1)

allnpti :: Bool -> Int -> [ULT]
allnpti True n = allnptiLR n
allnpti False n = allnptiRL n

-- a simple conjecture about extracting the interval of the Tamari lattice
-- corresponding to a normal planar indecomposable lambda term

-- verified for n<=6
conj1 :: Int -> Bool
conj1 n =
  let ts = allnptiLR n in
  let intervals = map (\t -> (C.rightleaf $ C.arcs2tree $ lams2arcsLR t,apps2tree t)) ts in
  length (nub intervals) == length intervals &&
  flip all intervals (\(c1,c2) -> T.tamari_order c1 c2)

-- false at n=2
conj2 :: Int -> Bool
conj2 n =
  let ts = allnptiRL n in
  let intervals = map (\t -> (C.rightleaf $ C.arcs2tree $ lams2arcsRL t,apps2tree t)) ts in
  length (nub intervals) == length intervals &&
  flip all intervals (\(c1,c2) -> T.tamari_order c1 c2)

-- false at n=2
conj3 :: Int -> Bool
conj3 n =
  let ts = allnptiLR n in
  let intervals = map (\t -> (C.arcs2tree $ type2arcsLR t,apps2tree t)) ts in
  length (nub intervals) == length intervals &&
  flip all intervals (\(c1,c2) -> T.tamari_order c1 c2)

-- false at n=3
conj4 :: Int -> Bool
conj4 n =
  let ts = allnptiRL n in
  let intervals = map (\t -> (C.rightleaf $ C.arcs2tree $ type2arcsRL t,apps2tree t)) ts in
  length (nub intervals) == length intervals &&
  flip all intervals (\(c1,c2) -> T.tamari_order c1 c2)

-- verified for n<=5
conj5 :: Int -> Bool
conj5 n =
  let ts = allcNPT True (n+1) in
  let pairs = map (\t -> (C.normalizeArcs $ lams2arcsLR t,apps2tree t)) ts in
  length (nub pairs) == length pairs

-- verified for n<=4
conj6 :: Int -> Bool
conj6 n =
  let ts = allcNLT (n+1) in
  let pairs = map (\t -> (C.normalizeArcs $ lams2arcsLR t,apps2tree t)) ts in
  length (nub pairs) == length pairs

-- verified for n<=4
conj7 :: Int -> Bool
conj7 n =
  let ts = allcNLT (n+1) in
  let pairs = map (\t -> (C.normalizeArcs $ lams2arcsRL t,apps2tree t)) ts in
  n == 0 || length (nub pairs) < length pairs

-- verified for n<=4
conj8 :: Int -> Bool
conj8 n =
  let ts = allcULT (toInteger $ 2*n+1) in
  let pairs = map (\t -> (C.normalizeArcs $ lams2arcsLR t,apps2tree t)) ts in
  n == 0 || length (nub pairs) < length pairs


-- verified for n<=6
conj9 :: Int -> Bool
conj9 n =
  let ts = allnptiRL n in
  let pairs = map (\t -> (C.normalizeArcs $ lams2arcsRL t,apps2tree t)) ts in
  n <= 1 || length (nub pairs) < length pairs

-- verified for n<=6
conj10 :: Int -> Bool
conj10 n =
  let ts = allnptiLR n in
  let pairs = map (\t -> (C.normalizeArcs $ lams2arcsRL t,apps2tree t)) ts in
  length (nub pairs) == length pairs

test11 :: Int -> [(C.Tree,C.Tree)]
test11 n =
  filter (\(t1,t2) -> isJust $ pluglamapp (C.tree2arcs t1) t2) [(t1,t2) | t1 <- C.binary_trees (n+1), t2 <- C.binary_trees n]
-- [length $ test11 n | n <- [0..]] == [1,2,9,54,378,2916,24057,...]

