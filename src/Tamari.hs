module Tamari where

import Data.List
import Catalan

rotR1 :: Catalan -> [Catalan]
rotR1 (B (t1 @ (B t11 t12)) t2) =
  B t11 (B t12 t2) : [B t1' t2 | t1' <- rotR1 t1] ++ [B t1 t2' | t2' <- rotR1 t2]
rotR1 (B L t2) = [B L t2' | t2' <- rotR1 t2]
rotR1 _ = []

rotL1 :: Catalan -> [Catalan]
rotL1 (B t1 (t2 @ (B t21 t22))) =
  B (B t1 t21) t22 : [B t1' t2 | t1' <- rotL1 t1] ++ [B t1 t2' | t2' <- rotL1 t2]
rotL1 (B t1 L) = [B t1' L | t1' <- rotL1 t1]
rotL1 _ = []

tamari_up :: Catalan -> [Catalan]
tamari_up t = t : foldr union [] [tamari_up t' | t' <- rotR1 t]

tamari_down :: Catalan -> [Catalan]
tamari_down t = t : foldr union [] [tamari_down t' | t' <- rotL1 t]

tamari_order :: Catalan -> Catalan -> Bool
tamari_order t1 t2 = elem t2 (tamari_up t1)

tamari_compare :: Catalan -> Catalan -> Ordering
tamari_compare t1 t2 = if t1 == t2 then EQ else if tamari_order t1 t2 then LT else GT

kreweras_order :: Catalan -> Catalan -> Bool
kreweras_order L L = True
kreweras_order (B t1 t2) (B t1' t2') =
  (kreweras_order t1 t1' && kreweras_order t2 t2') ||
  case t1 of
    B t11 t12 -> kreweras_order (B t11 (B t12 t2)) (B t1' t2')
    L -> False
kreweras_order _ _ = False

tamari :: Int -> [(Catalan,Catalan)]
tamari n = [(t1,t2) | t1 <- catalan n, t2 <- catalan n, tamari_order t1 t2]

kreweras :: Int -> [(Catalan,Catalan)]
kreweras n = [(t1,t2) | t1 <- catalan n, t2 <- catalan n, kreweras_order t1 t2]

tamari_parts :: Int -> [Int]
tamari_parts n = [length $ tamari_down t | t <- catalan n]
