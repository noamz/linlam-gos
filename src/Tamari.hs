module Tamari where

import Data.List
import Data.Maybe
import Catalan
import Bijections

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

-- sequent-style decision procedure for Tamari order
tamari_seq :: [Tree] -> Tree -> Tree -> Bool
tamari_seq g (B t1 t2) u = tamari_seq (t2:g) t1 u
tamari_seq g L L = g == []
tamari_seq g L (B u1 u2) =
  let k = leaves u1 in
  let grab k g acc =
        if k == 0 then Just (acc,g)
        else if g == [] then Nothing
        else
          let (t:g') = g in
          let i = leaves t in
          if i > k then Nothing
          else grab (k - i) g' (t:acc) in
  case grab (k-1) g [] of
    Nothing -> False
    Just (g1,t2:g2) -> tamari_seq (reverse g1) L u1 && tamari_seq g2 t2 u2
    Just (g1,[]) -> False

-- claim: tamari_seq agrees with tamari_order
-- verified for n<=6
prop2 :: Int -> Bool
prop2 n =
  flip all (binary_trees n) $ \t1 ->
  flip all (binary_trees n) $ \t2 ->
  tamari_order t1 t2 == tamari_seq [] t1 t2

-- focused sequent calculus
tamari_linv :: Tree -> [Tree] -> Tree -> Bool
tamari_neu :: [Tree] -> Tree -> Bool
tamari_linv t g u = let ts = tree2spine t in tamari_neu (reverse ts ++ g) u
tamari_neu g L = g == []
tamari_neu g (B u1 u2) =
  let k = leaves u1 in
  let grab k g acc =
        if k == 0 then Just (acc,g)
        else if g == [] then Nothing
        else
          let (t:g') = g in
          let i = leaves t in
          if i > k then Nothing
          else grab (k - i) g' (t:acc) in
  case grab (k-1) g [] of
    Nothing -> False
    Just (g1,t2:g2) -> tamari_neu (reverse g1) u1 && tamari_linv t2 g2 u2
    Just (g1,[]) -> False

-- verified for n<=7
prop3 :: Int -> Bool
prop3 n =
  flip all (binary_trees n) $ \t1 ->
  flip all (binary_trees n) $ \t2 ->
  tamari_linv t1 [] t2 == tamari_seq [] t1 t2

shuffle_linv :: Tree -> [Tree] -> Tree -> Bool
shuffle_neu :: [Tree] -> Tree -> Bool
shuffle_linv t g u =
  let ts = tree2spine t in
--  flip any (permutations ts) $ \ts' ->
  flip any (shuffle ts g) $ \g' ->
  shuffle_neu g' u
shuffle_neu g L = g == []
shuffle_neu g (B u1 u2) =
  let k = leaves u1 in
  let grab k g acc =
        if k == 0 then Just (acc,g)
        else if g == [] then Nothing
        else
          let (t:g') = g in
          let i = leaves t in
          if i > k then Nothing
          else grab (k - i) g' (t:acc) in
  case grab (k-1) g [] of
    Nothing -> False
    Just (g1,g2) ->
      flip any (remove g2) $ \(t2,g2') ->
      shuffle_neu (reverse g1) u1 && shuffle_linv t2 g2' u2

-- lattice structure
tamari_meetc :: [Tree] -> [Tree] -> [Tree]
tamari_meetc [] [] = []
tamari_meetc (B t1 t2:g) d = tamari_meetc (t1:t2:g) d
tamari_meetc g (B t1 t2:d) = tamari_meetc g (t1:t2:d)
tamari_meetc (L:g) (L:d) =
  let match = map fst $ fst $ break (uncurry (/=)) (zip g d) in
  let g' = fromJust $ stripPrefix match g in
  let d' = fromJust $ stripPrefix match d in
  L:match ++ tamari_meetc g' d'

-- tamari_join :: Tree -> Tree -> Tree
-- tamari_join t1 t2 =
--   if tamari_linv t1 [] t2 then t2 else
--   if tamari_linv t2 [] t1 then t1 else
--   let (cur1:g1) = tree2spine t1 in
--   let (cur2:g2) = tree2spine t2 in
--   match_and_join g1 g2 (leaves cur1,[cur1]) (leaves cur2,[cur2])
--   where
--     match_and_join :: [Tree] -> [Tree] -> (Int,[Tree]) -> (Int,[Tree]) -> Tree
--     match_and_join g1 g2 (k1,cur1) (k2,cur2) =
--       if k1 == k2 then
--         let j = tamari_join (tpsi cur1) (tpsi cur2) in
--         if g1 == g2 then j else let (t1:g1') = g1 in let (t2:g2') = g2 in B j (match_and_join g1' g2' (leaves t1,[t1]) (leaves t2,[t2]))
--       else if k1 < k2 then
--              let (t1:g1') = g1 in
--              match_and_join g1' g2 (k1+leaves t1,t1:cur1) (k2,cur2)
--            else
--              let (t2:g2') = g2 in
--              match_and_join g1 g2' (k1,cur1) (k2+leaves t2,t2:cur2)
--     tpsi :: [Tree] -> Tree
--     tpsi [t] = t
--     tpsi (t:ts) = foldl B t ts
                       
tree_type :: Tree -> [Bool]
tree_type t = pol False t
  where
    pol :: Bool -> Tree -> [Bool]
    pol b L = [b]
    pol b (B t1 t2) = pol False t1 ++ pol True t2
