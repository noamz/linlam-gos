{-# LANGUAGE NoMonomorphismRestriction #-}

module Viz.Catalan where

import Data.Maybe
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Catalan as C

spot :: Diagram B
spot = circle 0.01 # lwL 0.1 # fc black & pad 120

attach :: String -> String -> String -> Diagram B -> Diagram B
attach n1 n2 n =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
  let p = head $ intersectPoints
          (position [(location b1,fromOffsets [ 100*unitX # rotateBy (7/8)])] :: Path V2 Double)
          (position [(location b2,fromOffsets [ 100*unitX # rotateBy (5/8)])] :: Path V2 Double)
  in
   atop $
   position [(p,circle 0.2 # lwL 0.1 # fc black # named n & pad 3)]

-- given a name for the root together with a catalan structure, build a
-- binary tree diagram and return the list of names of the leaves
catTree :: String -> Catalan -> (Diagram B, [String])
catTree k L = (spot # named k, [k])
catTree k (B t1 t2) =
  let (d1,s1) = catTree ('L' : k) t1 in
  let (d2,s2) = catTree ('R' : k) t2 in
  let k1 = ('L' : k) in
  let k2 = ('R' : k) in
  (attach k1 k2 k (d1 ||| d2)
   # (connectOutside' (with & arrowHead .~ noHead) k k1 # lwL 0.1)
   # (connectOutside' (with & arrowHead .~ noHead) k k2 # lwL 0.1),
   s1 ++ s2)

diagCatTree :: Catalan -> Diagram B
diagCatTree c = fst $ catTree [] c

diagDyckArcs :: [Int] -> Diagram B
diagDyckArcs w =
  let pt n = spot # named n in
  let marked [] seen = []
      marked (x:xs) seen = if elem x seen then (True,x):marked xs seen else (False,x):marked xs (x:seen) in
  let w' = marked w [] in
  let shaft = arc xDir (-1/2 @@ turn) in
  hsep 1 (map pt w') #
  applyAll [connect' (with & arrowHead .~ noHead & arrowShaft .~ shaft) (False,i) (True,i) # lwL 0.1 | (False,i) <- w']

diagDyckArcs_glue :: [String] -> [Int] -> Diagram B -> Diagram B
diagDyckArcs_glue ns w d =
  let marked [] seen = []
      marked (x:xs) seen = if elem x seen then (True,x):marked xs seen else (False,x):marked xs (x:seen) in
  let w' = marked w [] in
  let dict = zip w' ns in
  let shaft = arc xDir (-1/2 @@ turn) in
  d #
  applyAll [connect' (with & arrowHead .~ noHead & arrowShaft .~ shaft) (fromJust $ lookup (False,i) dict) (fromJust $ lookup (True,i) dict) # lwL 0.1 | (False,i) <- w']

