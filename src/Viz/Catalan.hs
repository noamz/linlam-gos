{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Viz.Catalan where

import Data.Maybe

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Catalan as C

-- leaf :: Int -> Diagram B
-- leaf i = text (show i) # fontSizeL 0.2 # fc white <>
--          circle 0.2 # fc blue # named i

-- drawNode :: Bool -> Diagram B
-- drawNode isLeaf = circle 0.2 & pad 2 # fc (if isLeaf then blue else black)

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
  attach ('L' : k) ('R' : k) k (d1 ||| d2)
  # connectOutside' (with & arrowHead .~ noHead) (k) ('R' : k)
  # connectOutside' (with & arrowHead .~ noHead) (k) ('L' : k)

  -- ((d1 ||| d2) -- # centerX
  --  ===
  --  ((circle 0.2 & pad 2) # fc black # named k))
  -- # connectOutside' (with & arrowHead .~ noHead) k ('L' : k)
  -- # connectOutside' (with & arrowHead .~ noHead) k ('R' : k)
  -- where
  --   [p] = intersectPoints
  --         (arrowAt (location (fromJust $ lookupName ('L' : k))) (rotateBy (7/8) (10*unitX)))
  --         (arrowAt (location (fromJust $ lookupName ('R' : k))) (rotateBy (5/8) (10*unitX)))

