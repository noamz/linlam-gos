module Combinators where

import Data.List

import Lambda

cI :: ULT
cI = L 0 $ V 0

cB :: ULT
cB = L 0 $ L 1 $ L 2 $ A (V 0) $ A (V 1) $ V 2

cB' :: ULT
cB' = L 0 $ L 1 $ L 2 $ A (V 2) $ A (V 1) $ V 0

-- LR-planar term which normalizes to B
cBLR :: ULT
cBLR = L 0 $ L 1 $ L 2 $ A (A (A (L 3 $ L 4 $ L 5 $ A (V 5) $ A (V 4) $ V 3) (V 2)) (V 1)) (V 0)

cC :: ULT
cC = L 0 $ L 1 $ L 2 $ A (A (V 0) (V 2)) (V 1)

-- LR-planar term which normalizes to C
cCLR :: ULT
cCLR = L 0 $ L 1 $ L 2 $ A (A (A (L 3 $ L 4 $ A (A (L 5 $ L 6 $ L 7 $ A (A (V 7) (V 6)) (V 5)) (V 4)) (V 3)) (V 2)) (V 1)) (V 0)


cDNI :: ULT
cDNI = L 0 $ L 1 $ A (V 1) (V 0)

cstr :: ULT
cstr = L 0 $ L 1 $ L 2 $ A (V 0) (L 3 $ A (V 2) $ A (V 3) (V 1))

cPtensor :: ULT
cPtensor = L 0 $ L 1 $ L 2 $ A (V 0) (L 3 $ A (V 1) $ A (V 2) (V 3))

cPunit :: ULT
cPunit = L 0 $ A (V 0) $ L 1 $ V 1

cPt3 :: ULT
cPt3 = L 0 $ L 1 $ L 2 $ L 3 $ A (V 0) (L 4 $ A (V 1) $ A (V 2) $ A (V 3) $ V 4)

compose :: ULT -> ULT -> ULT
compose t u = L 0 (A t (A u (V 0)))

aclosure :: [ULT] -> [[ULT]]
aclosure basis =
  basis : iterate (\ts -> nubBy alpha $ map normalize [A t1 t2 | t1 <- ts, t2 <- ts]) basis

graded :: Int -> [ULT] -> [ULT]
graded n = filter (\t -> sizeNormal t == n)

aclosure_upto :: Int -> [ULT] -> [[ULT]]
aclosure_upto n basis =
  let basisn = filter (\t -> sizeNormal t <= n) basis in
  basisn : iterate (\ts -> filter (\t -> sizeNormal t <= n) $ nubBy alpha $ map normalize [A t1 t2 | t1 <- ts, t2 <- ts]) basisn

cclosure_upto :: Int -> [ULT] -> [[ULT]]
cclosure_upto n basis =
  let basisn = filter (\t -> sizeNormal t <= n) basis in
  basisn : iterate (\ts -> filter (\t -> sizeNormal t <= n) $ nubBy alpha $ map normalize [compose t1 t2 | t1 <- ts, t2 <- ts]) basisn
