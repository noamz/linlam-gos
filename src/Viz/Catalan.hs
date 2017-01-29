{-# LANGUAGE NoMonomorphismRestriction #-}

module Viz.Catalan where

import Data.Maybe
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Catalan

instance IsName Arc

spot :: Diagram B
spot = circle 0.01 # lwL 0.1 # fc black & pad 120

attach :: Colour Double -> String -> String -> String -> Diagram B -> Diagram B
attach col n1 n2 n =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
  let p = head $ intersectPoints
          (position [(location b1,fromOffsets [ 100*unitX # rotateBy (7/8)])] :: Path V2 Double)
          (position [(location b2,fromOffsets [ 100*unitX # rotateBy (5/8)])] :: Path V2 Double)
  in
   atop $
   position [(p,circle 0.2 # lwL 0.1 # fc col # named n & pad 3)]

-- given a name for the root together with a binary tree, build a
-- diagram and return the list of names of the leaves
diagTree_st :: Colour Double -> String -> Tree -> (Diagram B, [String])
diagTree_st col k L = (spot # named k, [k])
diagTree_st col k (B t1 t2) =
  let (d1,s1) = diagTree_st col ('L' : k) t1 in
  let (d2,s2) = diagTree_st col ('R' : k) t2 in
  let k1 = ('L' : k) in
  let k2 = ('R' : k) in
  (attach col k1 k2 k (d1 ||| d2)
   # (connectOutside' (with & arrowHead .~ noHead) k k1 # lwL 0.1)
   # (connectOutside' (with & arrowHead .~ noHead) k k2 # lwL 0.1),
   s1 ++ s2)

diagTree :: Colour Double -> Tree -> Diagram B
diagTree col cat =
  let nr = []::String in
  let t = fst $ diagTree_st col nr cat in
  (withName nr $ \b ->
   atop $ spot # named "root" # moveTo (location b) # translateY (-1))
  t # (connectOutside' (with & arrowHead .~ noHead) "root" nr # lwL 0.1)

diagArcs :: Arcs -> Diagram B
diagArcs w =
  let pt n = spot # named n in
  let shaft = arc xDir (-1/2 @@ turn) in
  hsep 1 (map pt w) #
  applyAll [connect' (with & arrowHead .~ noHead & arrowShaft .~ shaft) (U i) (D i) # lwL 0.1 | U i <- w]

diagArcs_glue :: [String] -> Arcs -> Diagram B -> Diagram B
diagArcs_glue ns w d =
  let dict = zip w ns in
  let shaft = arc xDir (-1/2 @@ turn) in
  d #
  applyAll [connect' (with & arrowHead .~ noHead & arrowShaft .~ shaft) (fromJust $ lookup (U i) dict) (fromJust $ lookup (D i) dict) # lwL 0.1 | U i <- w]

bigspot :: Diagram B
bigspot = circle 0.3 # lwL 0.1 # fc black & pad 4

diagArcs' :: Arcs -> Diagram B
diagArcs' w =
  let pt n = bigspot # named n in
  let shaft = arc xDir (-1/2 @@ turn) in
  hsep 1 (map pt w) #
  applyAll [connect' (with & arrowHead .~ noHead & arrowShaft .~ shaft) (U i) (D i) # lwL 0.1 | U i <- w]
