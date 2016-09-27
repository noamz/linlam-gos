{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Viz.Lambda where

import Data.Maybe
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import qualified Catalan as C
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
  let w = Ch.lams2arcs b t in
  VC.diagArcs_glue ns (if b then w else C.fliparcs $ reverse w) d

typeTree :: String -> L.Type -> (Diagram B, [String])
typeTree k (L.TVar _) = (spot # named k, [k])
typeTree k (L.TFn t1 t2) =
  let (d1,s1) = typeTree ('L' : k) t1 in
  let (d2,s2) = typeTree ('R' : k) t2 in
  let k1 = ('L' : k) in
  let k2 = ('R' : k) in
  (attach False k1 k2 k (d1 ||| d2)
   # (connectOutside' (with & arrowHead .~ noHead) k k1 # lwL 0.1)
   # (connectOutside' (with & arrowHead .~ noHead) k k2 # lwL 0.1),
   s1 ++ s2)

posTree :: Bool -> String -> L.Type -> (Diagram B, [String])
negTree :: Bool -> String -> L.Type -> (Diagram B, [String])
posTree b k (L.TVar _) = (spot # named k, [k])
posTree b k (L.TFn t1 t2) =
  let (d1,s1) = if b then negTree b ('L' : k) t1 else posTree b ('L' : k) t2 in
  let (d2,s2) = if b then posTree b ('R' : k) t2 else negTree b ('R' : k) t1 in
  let k1 = ('L' : k) in
  let k2 = ('R' : k) in
  (attach False k1 k2 k (d1 ||| d2)
   # (connectOutside' (with & arrowHead .~ noHead) k k1 # lwL 0.1)
   # (connectOutside' (with & arrowHead .~ noHead) k k2 # lwL 0.1),
   s1 ++ s2)
negTree b k (L.TVar _) = (spot # named k, [k])
negTree b k (L.TFn t1 t2) =
  let (d1,s1) = posTree b ('L' : k) t1 in
  let (d2,s2) = negTree b ('R' : k) t2 in
  let k1 = ('L' : k) in
  let k2 = ('R' : k) in
  (attach True k1 k2 k (d1 ||| d2)
   # (connectOutside' (with & arrowHead .~ noHead) k k1 # lwL 0.1)
   # (connectOutside' (with & arrowHead .~ noHead) k k2 # lwL 0.1),
   s1 ++ s2)

diagPos :: L.Type -> Diagram B
diagPos t =
  let (d,ns) = posTree False [] t in
  let w = C.dow2arcs (L.linearizePos t) in
  VC.diagArcs_glue ns w d

diagType :: Bool -> L.Type -> Diagram B
diagType b t =
  let (d,ns) = posTree b [] t in
  let w = C.dow2arcs (if b then L.linearizeType t else L.linearizePos t) in
  VC.diagArcs_glue ns w d
