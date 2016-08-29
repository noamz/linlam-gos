module Chapo where

import Data.List

import Bijections
import Lambda
import qualified Catalan as C
import qualified Tamari as T

-- extract a catalan object from a lambda term by looking at its
-- underlying applicative structure

apps2cat :: ULT -> C.Catalan
apps2cat (L _ t) = apps2cat t
apps2cat (A t1 t2) = C.B (apps2cat t1) (apps2cat t2)
apps2cat (V _) = C.L

-- extract a catalan object from a (closed) planar lambda term by
-- looking at its underlying binding structure (we can use either
-- the LR or the RL planarity convention)

lams2dowLR :: ULT -> [Int]
lams2dowLR (L x t) = x : lams2dowLR t
lams2dowLR (A t1 t2) = lams2dowLR t1 ++ lams2dowLR t2
lams2dowLR (V x) = [x]

lams2dowRL :: ULT -> [Int]
lams2dowRL (L x t) = x : lams2dowRL t
lams2dowRL (A t1 t2) = lams2dowRL t2 ++ lams2dowRL t1
lams2dowRL (V x) = [x]

lams2dow :: Bool -> ULT -> [Int]
lams2dow True t = lams2dowLR t
lams2dow False t = lams2dowRL t

lams2catLR :: ULT -> C.Catalan
lams2catLR t = C.dyck2cat (lams2dowLR t)

lams2catRL :: ULT -> C.Catalan
lams2catRL t = C.dyck2cat (lams2dowRL t)

-- extract a catalan object from a (closed) planar lambda term by
-- looking at its principal type

type2catLR :: ULT -> C.Catalan
type2catLR t = C.dycks2cat (linearizeType $ synthClosedNormal t)

type2catRL :: ULT -> C.Catalan
type2catRL t = C.dyck2cat (linearizePos $ synthClosedNormal t)

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

conj1 :: Int -> Bool
conj1 n =
  let ts = allnptiLR n in
  let intervals = map (\t -> (lams2catLR t,apps2cat t)) ts in
  length (nub intervals) == length intervals &&
  flip all intervals (\(c1,c2) -> T.tamari_order c1 c2)

conj2 :: Int -> Bool
conj2 n =
  let ts = allnptiRL n in
  let intervals = map (\t -> (lams2catRL t,apps2cat t)) ts in
  length (nub intervals) == length intervals &&
  flip all intervals (\(c1,c2) -> T.tamari_order c1 c2)

conj3 :: Int -> Bool
conj3 n =
  let ts = allnptiLR n in
  let intervals = map (\t -> (type2catLR t,apps2cat t)) ts in
  length (nub intervals) == length intervals &&
  flip all intervals (\(c1,c2) -> T.tamari_order c1 c2)

conj4 :: Int -> Bool
conj4 n =
  let ts = allnptiRL n in
  let intervals = map (\t -> (type2catRL t,apps2cat t)) ts in
  length (nub intervals) == length intervals &&
  flip all intervals (\(c1,c2) -> T.tamari_order c1 c2)

