{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Viz.Catalan where

import Data.Maybe

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

diagTree :: Catalan -> String -> Diagram B
diagTree L k = (circle 0.2 # fc blue & pad 3) # named k
diagTree (B t1 t2) k =
  let d1 = diagTree t1 ('L' : k) in
  let d2 = diagTree t2 ('R' : k) in
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

