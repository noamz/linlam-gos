{-# LANGUAGE DeriveDataTypeable #-}

module TreeCol where

import Data.List
import Data.Maybe
import Data.Typeable

import Bijections
import Catalan

data Treep p = LL | BB p (Treep p) (Treep p)
  deriving (Show,Eq,Ord,Typeable)

type Tree2 = Treep (Either () ())

pbinary_trees :: [n] -> Int -> [Treep n]
pbinary_trees ls 0 = [LL]
pbinary_trees ls n = [BB l t1 t2 | l <- ls, i <- [0..n-1], t1 <- pbinary_trees ls i, t2 <- pbinary_trees ls (n-1-i)]

binary_trees2c = pbinary_trees [Left (), Right ()]

oseq :: [Tree2] -> Tree2 -> Bool
oseq g (BB (Left ()) t1 t2) = oseq (t1 : g) t2
oseq g u =
  (u == LL && g == [LL]) ||
  (flip any [0..length g-1] $ \i ->
    let (g1,t:g2) = splitAt i g in
    case t of
      BB (Right ()) t1 t2 ->
        flip any [0..length g2] $ \j ->
        let (g21,g22) = splitAt j g2 in
        oseq (g1 ++ [t1] ++ g22) u && oseq g21 t2
      _ -> False)

rinv :: [Tree2] -> Tree2 -> Bool
neu :: [Tree2] -> Tree2 -> Bool
lfoc :: [Tree2] -> Tree2 -> [Tree2] -> Tree2 -> Bool

rinv g (BB (Left ()) t1 t2) = rinv (t1 : g) t2
rinv g u = neu g u
neu g u =
  flip any [0..length g-1] $ \i ->
  let (g1,t:g2) = splitAt i g in
  lfoc g1 t g2 u
lfoc g1 LL g2 u = g1 == [] && g2 == [] && u == LL
lfoc g1 (BB (Right ()) t1 t2) g2 u =
  flip any [0..length g2] $ \j ->
--  flip any [1..length g2] $ \j ->
  let (g21,g22) = splitAt j g2 in
  lfoc g1 t1 g22 u && rinv g21 t2
lfoc g1 _ g2 u = False
