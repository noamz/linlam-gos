{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Viz.Lambda where

import Data.Maybe
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import qualified Lambda as L
import qualified Chapo as Ch

import qualified Viz.Catalan as VC

attach :: Bool -> String -> String -> String -> Diagram B -> Diagram B
attach isApp n1 n2 n =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
  let p = head $ intersectPoints
          (position [(location b1,fromOffsets [ 100*unitX # rotateBy (7/8)])] :: Path V2 Double)
          (position [(location b2,fromOffsets [ 100*unitX # rotateBy (5/8)])] :: Path V2 Double)
  in
   atop $
   position [(p,circle 0.2 # lwL 0.1 # fc (if isApp then black else red) # named n & pad 3)]

spot :: Diagram B
spot = circle 0.01 # lwL 0.1 # fc black & pad 120

lamTree :: String -> Bool -> L.ULT -> (Diagram B, [String])
lamTree k b (L.V _) = (spot # named k, [k])
lamTree k b (L.A t1 t2) =
  let (d1,s1) = lamTree ('L' : k) b t1 in
  let (d2,s2) = lamTree ('R' : k) b t2 in
  let k1 = ('L' : k) in
  let k2 = ('R' : k) in
  (attach True k1 k2 k (d1 ||| d2)
   # (connectOutside' (with & arrowHead .~ noHead) k k1 # lwL 0.1)
   # (connectOutside' (with & arrowHead .~ noHead) k k2 # lwL 0.1),
   s1 ++ s2)
lamTree k True (L.L x t) =
  let (d1 :: Diagram B,s1) = (spot # named ('L' : k), ['L': k]) in
  let (d2,s2) = lamTree ('R' : k) True t in
  let k1 = ('L' : k) in
  let k2 = ('R' : k) in
  (attach False k1 k2 k (d1 ||| d2)
   # (connectOutside' (with & arrowHead .~ noHead) k k1 # lwL 0.1)
   # (connectOutside' (with & arrowHead .~ noHead) k k2 # lwL 0.1),
   s1 ++ s2)
lamTree k False (L.L x t) =
  let (d1,s1) = lamTree ('L' : k) False t in
  let (d2 :: Diagram B,s2) = (spot # named ('R' : k), ['R': k]) in
  let k1 = ('L' : k) in
  let k2 = ('R' : k) in
  (attach False k1 k2 k (d1 ||| d2)
   # (connectOutside' (with & arrowHead .~ noHead) k k1 # lwL 0.1)
   # (connectOutside' (with & arrowHead .~ noHead) k k2 # lwL 0.1),
   s1 ++ s2)

diagULT :: Bool -> L.ULT -> Diagram B
diagULT b t =
  let (d,ns) = lamTree [] b t in
  VC.diagDyckArcs_glue ns (Ch.lams2dow b t) d


