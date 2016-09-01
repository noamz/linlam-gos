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

lams2dow :: Bool -> ULT -> C.Arcs
lams2dow b (L x t) = C.U x : lams2dow b t
lams2dow b (A t1 t2) = if b then lams2dow b t1 ++ lams2dow b t2
                       else lams2dow b t2 ++ lams2dow b t1
lams2dow b (V x) = [C.D x]

lams2dowLR = lams2dow True
lams2dowRL = lams2dow False

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
  let intervals = map (\t -> (C.rightleaf $ C.arcs2tree $ lams2dowLR t,apps2tree t)) ts in
  length (nub intervals) == length intervals &&
  flip all intervals (\(c1,c2) -> T.tamari_order c1 c2)

-- false at n=2
conj2 :: Int -> Bool
conj2 n =
  let ts = allnptiRL n in
  let intervals = map (\t -> (C.rightleaf $ C.arcs2tree $ lams2dowRL t,apps2tree t)) ts in
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
  let pairs = map (\t -> (C.normalizeArcs $ lams2dowLR t,apps2tree t)) ts in
  length (nub pairs) == length pairs

-- verified for n<=4
conj6 :: Int -> Bool
conj6 n =
  let ts = allcNLT (n+1) in
  let pairs = map (\t -> (C.normalizeArcs $ lams2dowLR t,apps2tree t)) ts in
  length (nub pairs) == length pairs

-- verified for n<=4
conj7 :: Int -> Bool
conj7 n =
  let ts = allcNLT (n+1) in
  let pairs = map (\t -> (C.normalizeArcs $ lams2dowRL t,apps2tree t)) ts in
  n == 0 || length (nub pairs) < length pairs

-- verified for n<=4
conj8 :: Int -> Bool
conj8 n =
  let ts = allcULT (toInteger $ 2*n+1) in
  let pairs = map (\t -> (C.normalizeArcs $ lams2dowLR t,apps2tree t)) ts in
  n == 0 || length (nub pairs) < length pairs


-- verified for n<=6
conj9 :: Int -> Bool
conj9 n =
  let ts = allnptiRL n in
  let pairs = map (\t -> (C.normalizeArcs $ lams2dowRL t,apps2tree t)) ts in
  n <= 1 || length (nub pairs) < length pairs

-- verified for n<=6
conj10 :: Int -> Bool
conj10 n =
  let ts = allnptiLR n in
  let pairs = map (\t -> (C.normalizeArcs $ lams2dowRL t,apps2tree t)) ts in
  length (nub pairs) == length pairs
