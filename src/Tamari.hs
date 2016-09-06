module Tamari where

import Data.List
import Catalan

rotR1 :: Tree -> [Tree]
rotR1 (B (t1 @ (B t11 t12)) t2) =
  B t11 (B t12 t2) : [B t1' t2 | t1' <- rotR1 t1] ++ [B t1 t2' | t2' <- rotR1 t2]
rotR1 (B L t2) = [B L t2' | t2' <- rotR1 t2]
rotR1 _ = []

rotL1 :: Tree -> [Tree]
rotL1 (B t1 (t2 @ (B t21 t22))) =
  B (B t1 t21) t22 : [B t1' t2 | t1' <- rotL1 t1] ++ [B t1 t2' | t2' <- rotL1 t2]
rotL1 (B t1 L) = [B t1' L | t1' <- rotL1 t1]
rotL1 _ = []

tamari_up :: Tree -> [Tree]
tamari_up t = t : foldr union [] [tamari_up t' | t' <- rotR1 t]

tamari_down :: Tree -> [Tree]
tamari_down t = t : foldr union [] [tamari_down t' | t' <- rotL1 t]

tamari_order :: Tree -> Tree -> Bool
tamari_order t1 t2 = elem t2 (tamari_up t1)

kreweras_order :: Tree -> Tree -> Bool
kreweras_order L L = True
kreweras_order (B t1 t2) (B t1' t2') =
  (kreweras_order t1 t1' && kreweras_order t2 t2') ||
  case t1 of
    B t11 t12 -> kreweras_order (B t11 (B t12 t2)) (B t1' t2')
    L -> False
kreweras_order _ _ = False

tamari :: Int -> [(Tree,Tree)]
tamari n = [(t1,t2) | t1 <- binary_trees n, t2 <- tamari_up t1]
-- [length $ tamari n | n <- [0..]] == [1,1,3,13,68,399,2530,...]

kreweras :: Int -> [(Tree,Tree)]
kreweras n = [(t1,t2) | t1 <- binary_trees n, t2 <- binary_trees n, kreweras_order t1 t2]

tamari_parts :: Int -> [Int]
tamari_parts n = [length $ tamari_down t | t <- binary_trees n]

-- some properties of the Tamari lattice

-- If t<=u in the Tamari order, then the left-branching spine of t is at
-- least as long as the left-branching spine of u.
-- verified for n<=6
prop1 :: Int -> Bool
prop1 n =
  flip all (tamari n) $ \(t1,t2) ->
  length (tree2spine t1) >= length (tree2spine t2)
  
