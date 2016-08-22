{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Viz.Catalan where

import Data.Maybe
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Catalan as C

attach :: String -> String -> String -> Diagram B -> Diagram B
attach n1 n2 n =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
  let p = head $ intersectPoints
            (position [(location b1,fromOffsets [ 100*unitX # rotateBy (7/8)])] :: Path V2 Double)
            (position [(location b2,fromOffsets [ 100*unitX # rotateBy (5/8)])] :: Path V2 Double)
  in
   atop $
   (((position [(p,circle 0.2 # fc black # named n & pad 3)]))
    # connectOutside' (with & arrowHead .~ noHead) n n1
    # connectOutside' (with & arrowHead .~ noHead) n n2)

diagCatTree :: String -> Catalan -> Diagram B
diagCatTree k L = (circle 0.2 # fc blue & pad 3) # named k
diagCatTree k (B t1 t2) =
  let d1 = diagCatTree ('L' : k) t1 in
  let d2 = diagCatTree ('R' : k) t2 in
  let k1 = ('L' : k) in
  let k2 = ('R' : k) in
  withName k1 (\b1 ->
   withName k2 $ \b2 ->
   let p = head $ intersectPoints
           (position [(location b1,fromOffsets [ 100*unitX # rotateBy (7/8)])] :: Path V2 Double)
           (position [(location b2,fromOffsets [ 100*unitX # rotateBy (5/8)])] :: Path V2 Double)
   in
    atop $
    (position [(p,circle 0.2 # fc black # named k)])) $
  attach k1 k2 k (d1 ||| d2)
  # connectOutside' (with & arrowHead .~ noHead) k k1
  # connectOutside' (with & arrowHead .~ noHead) k k2

diagDyckArcs :: [Int] -> Diagram B
diagDyckArcs w =
  let pt n = circle 0.2 # fc black # named n in
  let marked [] seen = []
      marked (x:xs) seen = if elem x seen then (True,x):marked xs seen else (False,x):marked xs (x:seen) in
  let w' = marked w [] in
  let shaft = arc xDir (-1/2 @@ turn) in
  hsep 1 (map pt w') #
  applyAll [connect' (with & arrowHead .~ noHead & arrowShaft .~ shaft) (False,i) (True,i) | (False,i) <- w']


