module Chapo where

import Data.List
import Data.Maybe

import Control.Monad.State
import Util

import Formulas
import Bijections
import Lambda
import Scott1980

import qualified Catalan as C
import qualified Tamari as T
import qualified Maps as M

-- extract a binary tree from a lambda term by looking at its
-- underlying applicative structure

apps2tree :: ULT -> C.Tree
apps2tree (L _ t) = apps2tree t
apps2tree (A t1 t2) = C.B (apps2tree t1) (apps2tree t2)
apps2tree (V _) = C.L

-- extract an arc diagram from a linear lambda term by
-- looking at its underlying binding structure (we can use either
-- the LR or the RL planarity convention to build the arc diagram)

lams2arcs :: Bool -> ULT -> C.Arcs
lams2arcs b (L x t) = C.U x : lams2arcs b t
lams2arcs b (A t1 t2) = if b then lams2arcs b t1 ++ lams2arcs b t2
                       else lams2arcs b t2 ++ lams2arcs b t1
lams2arcs b (V x) = [C.D x]

lams2arcsLR = lams2arcs True
lams2arcsRL = lams2arcs False

isPlanarLR :: ULT -> Bool
isPlanarLR t = C.isDyck (lams2arcsLR t)
isPlanarRL :: ULT -> Bool
isPlanarRL t = C.isDyck (lams2arcsRL t)

-- given a double-occurrence word representing its binding structure,
-- and a binary tree representing its applicative structure, attempt
-- to reconstruct a normal linear term

arcstree2nlt :: C.Arcs -> C.Tree -> Maybe ULT
arcstree2nlt (C.U x:w) c =
  if (C.D x) `elem` w then do
    u <- arcstree2nlt w c
    return $ L x u
  else Nothing
arcstree2nlt [C.D x] C.L = return $ V x
arcstree2nlt (C.D x:w) (C.B c1 c2) = do
  let k = C.leaves c1
  let (w1,w2) = splitarcs k (C.D x:w)
  u1 <- arcstree2nlt w1 c1
  u2 <- arcstree2nlt w2 c2
  return $ A u1 u2
  where
    splitarcs :: Int -> C.Arcs -> (C.Arcs,C.Arcs)
    splitarcs 0 w = ([],w)
    splitarcs n (C.U x:w) = let (w1,w2) = splitarcs n w in (C.U x:w1,w2)
    splitarcs n (C.D x:w) = let (w1,w2) = splitarcs (n-1) w in (C.D x:w1,w2)

-- reconstruct a normal "pseudo"-term (this always succeeds)

arcstree2pseudonlt :: C.Arcs -> C.Tree -> ULT
arcstree2pseudonlt (C.U x:w) c = L x (arcstree2pseudonlt w c)
arcstree2pseudonlt [C.D x] C.L = V x
arcstree2pseudonlt (C.D x:w) (C.B c1 c2) =
  let k = C.leaves c1 in
  let (w1,w2) = splitarcs k (C.D x:w) in
  let u1 = arcstree2pseudonlt w1 c1 in
  let u2 = arcstree2pseudonlt w2 c2 in
  A u1 u2
  where
    splitarcs :: Int -> C.Arcs -> (C.Arcs,C.Arcs)
    splitarcs 0 w = ([],w)
    splitarcs n (C.U x:w) = let (w1,w2) = splitarcs n w in (C.U x:w1,w2)
    splitarcs n (C.D x:w) = let (w1,w2) = splitarcs (n-1) w in (C.D x:w1,w2)

pseudonpts :: Int -> [ULT]
pseudonpts n =
  map (uncurry arcstree2pseudonlt) $
  filter (\(w1,t2) -> isNothing $ arcstree2nlt w1 t2)
  [(w1,t2) |
   t1 <- C.binary_trees (n+1),
   let w1 = C.tree2arcs t1,
   t2 <- C.binary_trees n]

pseudonlts :: Int -> [ULT]
pseudonlts n =
  map (uncurry arcstree2pseudonlt) $
  filter (\(w1,t2) -> isNothing $ arcstree2nlt w1 t2)
  [(w1,t2) |
   f1 <- involute [0..2*(n+1)-1],
   let w1 = C.inv2arcs f1,
   t2 <- C.binary_trees n]

-- extract an arc diagram from a normal linear lambda term by
-- looking at its principal type

type2arcsLR :: ULT -> C.Arcs
type2arcsLR t =
  C.dow2arcs $ (reverse . drop 2 . reverse) $ linearizeType $ synthClosedNormal t

type2arcsRL :: ULT -> C.Arcs
type2arcsRL t = C.dow2arcs $ linearizePos $ synthClosedNormal t

-- the number of normal planar indecomposable lambda terms of size n+1 is
-- equal to the number of intervals in the Tamari lattice T_n.

allnptiRL :: Int -> [ULT]
allnptiRL n = allcNPTnb False (n+1)

allnptiLR :: Int -> [ULT]
allnptiLR n = allcNPTnb True (n+1)

allnpti :: Bool -> Int -> [ULT]
allnpti True n = allnptiLR n
allnpti False n = allnptiRL n

-- a simple conjecture about extracting the interval of the Tamari lattice
-- corresponding to a normal planar indecomposable lambda term

-- verified for n<=6
conj1 :: Int -> Bool
conj1 n =
  let ts = allnptiLR n in
  let intervals = map (\t -> (C.rightleaf $ C.arcs2tree $ lams2arcsLR t,apps2tree t)) ts in
  length (nub intervals) == length intervals &&
  flip all intervals (\(c1,c2) -> T.tamari_order c1 c2)

-- false at n=2
conj2 :: Int -> Bool
conj2 n =
  let ts = allnptiRL n in
  let intervals = map (\t -> (C.rightleaf $ C.arcs2tree $ lams2arcsRL t,apps2tree t)) ts in
  length (nub intervals) == length intervals &&
  flip all intervals (\(c1,c2) -> T.tamari_order c1 c2)

-- false at n=2
conj3 :: Int -> Bool
conj3 n =
  let ts = allnptiLR n in
  let intervals = map (\t -> (C.arcs2tree $ type2arcsLR t,apps2tree t)) ts in
  length (nub intervals) == length intervals &&
  flip all intervals (\(c1,c2) -> T.tamari_order c1 c2)

-- false at n=3
conj4 :: Int -> Bool
conj4 n =
  let ts = allnptiRL n in
  let intervals = map (\t -> (C.rightleaf $ C.arcs2tree $ type2arcsRL t,apps2tree t)) ts in
  length (nub intervals) == length intervals &&
  flip all intervals (\(c1,c2) -> T.tamari_order c1 c2)

-- verified for n<=5
conj5 :: Int -> Bool
conj5 n =
  let ts = allcNPT True (n+1) in
  let pairs = map (\t -> (C.normalizeArcs $ lams2arcsLR t,apps2tree t)) ts in
  length (nub pairs) == length pairs

-- verified for n<=4
conj6 :: Int -> Bool
conj6 n =
  let ts = allcNLT (n+1) in
  let pairs = map (\t -> (C.normalizeArcs $ lams2arcsLR t,apps2tree t)) ts in
  length (nub pairs) == length pairs

-- verified for n<=4
conj7 :: Int -> Bool
conj7 n =
  let ts = allcNLT (n+1) in
  let pairs = map (\t -> (C.normalizeArcs $ lams2arcsRL t,apps2tree t)) ts in
  n == 0 || length (nub pairs) < length pairs

-- verified for n<=4
conj8 :: Int -> Bool
conj8 n =
  let ts = allcULT (2*n+1) in
  let pairs = map (\t -> (C.normalizeArcs $ lams2arcsLR t,apps2tree t)) ts in
  n == 0 || length (nub pairs) < length pairs


-- verified for n<=6
conj9 :: Int -> Bool
conj9 n =
  let ts = allnptiRL n in
  let pairs = map (\t -> (C.normalizeArcs $ lams2arcsRL t,apps2tree t)) ts in
  n <= 1 || length (nub pairs) < length pairs

-- verified for n<=6
conj10 :: Int -> Bool
conj10 n =
  let ts = allnptiLR n in
  let pairs = map (\t -> (C.normalizeArcs $ lams2arcsRL t,apps2tree t)) ts in
  length (nub pairs) == length pairs

test11 :: Int -> [(C.Tree,C.Tree)]
test11 n =
  filter (\(t1,t2) -> isJust $ arcstree2nlt (C.tree2arcs t1) t2) [(t1,t2) | t1 <- C.binary_trees (n+1), t2 <- C.binary_trees n]
-- [length $ test11 n | n <- [0..]] == [1,2,9,54,378,2916,24057,...]


test12 :: Int -> [(C.Arcs,C.Tree)]
test12 n =
  filter (\(w1,t2) -> isJust $ arcstree2nlt w1 t2)
  [(w1,t2) |
   f1 <- involute [0..2*(n+1)-1],
   let w1 = C.inv2arcs f1,
   t2 <- C.binary_trees n]
-- [length $ test12 n | n <- [0..]] == [1,3,26,367,7142,...]

seq1_acc :: ([Int],C.Arcs) -> C.Tree -> Maybe ([Int],C.Arcs)
seq1_acc (g,(C.U x:w)) c = do
  (g',w') <- seq1_acc ((x:g),w) c
  if x `elem` g' then Nothing else return (g',w')
seq1_acc (x:g, C.D x':w) C.L = if x == x' then Just (g,w) else Nothing
seq1_acc (x:g, C.D x':w) (C.B c1 c2) =
  if x == x' then do
    (g',w') <- seq1_acc (x:g, C.D x':w) c1
    (g'',w'') <- seq1_acc (g',w') c2
    return (g'',w'')
  else Nothing
seq1_acc _ _ = Nothing

seq1 :: C.Arcs -> C.Tree -> Bool
seq1 w c = seq1_acc ([],w) c == Just ([],[])

test13 :: Int -> [(C.Arcs,C.Tree)]
test13 n =
  filter (uncurry seq1) [(w1,t2) | t1 <- C.binary_trees (n+1), let w1 = C.tree2arcs t1, t2 <- C.binary_trees n]

-- [length $ test13 n | n <- [0..]] == [1,2,9,54,378,2916,24057,...]

seq2_acc :: ([Int],C.Arcs) -> C.Tree -> Maybe ([Int],C.Arcs)
seq2_acc (y:g,(C.U x:w)) c = do
  (g',w') <- seq2_acc ((x:y:g),w) c
  if x `elem` g' || y `elem` g' then Nothing else return (g',w')
seq2_acc (x:g, C.D x':w) C.L = if x == x' then Just (g,w) else Nothing
seq2_acc (x:g, C.D x':w) (C.B c1 c2) =
  if x == x' then do
    (g',w') <- seq2_acc (x:g, C.D x':w) c1
    (g'',w'') <- seq2_acc (g',w') c2
    return (g'',w'')
  else Nothing
seq2_acc _ _ = Nothing

seq2 :: C.Arcs -> C.Tree -> Bool
seq2 (C.U x:w) c = seq2_acc ([x],w) c == Just ([],[])

test14 :: Int -> [(C.Arcs,C.Tree)]
test14 n =
  filter (uncurry seq2) [(w1,t2) | t1 <- C.binary_trees (n+1), let w1 = C.tree2arcs t1, t2 <- C.binary_trees n]

-- [length $ test14 n | n <- [0..]] == [1,1,3,13,68,399,2530,...]

-- verified for n<=6
conj15 :: Int -> Bool
conj15 n =
  let ts = allnptiLR n in
  let byvars = map (\t ->  length (fst $ unlambdas t)) ts in
  let byspine = map (\(t1,t2) -> 1 + length (C.tree2spine t1)) (T.tamari n) in
  sort byvars == sort byspine

-- verified for n<=6
conj16 :: Int -> Bool
conj16 n =
  let ts = allnptiLR n in
  let byvarsapps = map (\t -> let (g,u) = unlambdas t in (length g, length (snd $ unapps u []))) ts in
  let byspines = map (\(t1,t2) -> (1 + length (C.tree2spine t1), length (C.tree2spine t2))) (T.tamari n) in
  sort byvarsapps == sort byspines

chapo17 :: Int -> [ULT]
chapo17 n = [t | t <- allnptiLR n, let (g,u) = unlambdas t in length (snd $ unapps u []) == 1]

-- [length $ chapo17 n | n <- [0..]] == [0,1,2,8,41,240,1528,...]
-- cf. Chapoton, Section 5, Equation (12)

-- verified for n<=7
conj18 :: Int -> Bool
conj18 n = length (idempotents $ allnptiRL n) == catalan n

-- other computations:
-- [length (idempotents $ allcNPT False n) | n <- [1..]] == [1,1,2,6,21,83,353,...]
-- [length (idempotents $ allcNPT True n) | n <- [1..]] == [1,0,1,1,9,23,163,...]
-- [length (idempotents $ allcNLT n) | n <- [1..]] == [1,1,3,13,72,477,...]

-- verified for n<=7
conj19 :: Int -> Bool
conj19 n = sort (map apps2tree $ idempotents $ allnptiRL n) == sort (C.binary_trees n)

-- verified for n<=7
conj19' :: Int -> Bool
conj19' n =
  let toLR t = fromJust $ find (\t' -> ult2skel t == ult2skel t') (allnptiLR n) in
  all (\t -> apps2tree t == C.rightleaf (C.arcs2tree (lams2arcsLR $ toLR t))) (idempotents $ allnptiRL n)
  

-- false at n==4
conj20 :: Int -> Bool
conj20 n =
  let ts = allnptiRL n in
  let typetree t = C.rightleaf $ C.arcs2tree $ type2arcsRL t in
  let bytypes = equivClassesBy (\t1 t2 -> typetree t1 == typetree t2) ts [] in
  let downsets = map T.tamari_down (C.binary_trees n) in
  (sort $ map length bytypes) == (sort $ map length downsets)

-- verified for n<=7
conj21 :: Int -> Bool
conj21 n =
  let props = idempotents $ allnptiRL n in
  all id [beta (compose i j) (compose j i) | i <- props, j <- props]

-- false at n==3
conj22 :: Int -> Bool
conj22 n =
  let props = idempotents $ allcNPT False (n+1) in
  all id [beta (compose i j) (compose j i) | i <- props, j <- props]

-- false at n==3
conj23 :: Int -> Bool
conj23 n =
  let props = idempotents $ allcNLTnb (n+1) in
  all id [beta (compose i j) (compose j i) | i <- props, j <- props]

-- verified for n<=7
conj24 :: Int -> Bool
conj24 n = length (filter (eta (L 0 $ V 0)) $ allnptiRL n) == catalan n

-- verified for n<=5
conj25 :: Int -> Bool
conj25 n =
  let ts = filter isPlanarLR $ allcULTnb (2*n+1) in
  let intervals = map (\t -> (C.rightleaf $ C.arcs2tree $ lams2arcsLR t,apps2tree t)) ts in
  flip all intervals (\(c1,c2) -> T.tamari_seq [] c1 c2)

termint :: ULT -> (C.Tree,C.Tree)
termint t = (C.rightleaf $ C.arcs2tree $ lams2arcsLR t,apps2tree t)

vartps :: ULT -> TCtx
vartps t =
  let (_,t') = unlambdas t in
  let (_,_,g,_) = synth t' in
  g

termsig :: ULT -> [Int]
termsig t =
  map length $ map (\(x,tp) -> linearizeType tp) (vartps t)

conj26 :: Int -> [((C.Tree,C.Tree),[Int])]
conj26 n =
  let ts = allnptiLR n in
  zip (map termint ts) (map termsig ts)

conj27 :: Int -> [[Int]]
conj27 n =
  flip map (T.tamari n) $ \(t1,t2) ->
  map (uncurry (+)) $
  zip (map (C.ldepth . reverse) (C.paths t1)) (map (C.ldepth . reverse) (C.paths t2))

-- Some more observations/conjectures:
-- [length $ nub $ map lams2arcsLR $ allcNPT True n | n <- [1..]] == [1,2,5,14,42,132,429,...] == A000108 (offset 1)
-- [length $ nub $ map lams2arcsLR $ allcNPTnb True n | n <- [1..]] == [1,1,2,5,14,42,132,...] == A000108 (offset 0)
-- [length $ nub $ map lams2arcsLR $ allcNLT n | n <- [1..]] == [1,3,15,105,945,...] == A001147 (offset 1)
-- [length $ nub $ map lams2arcsLR $ allcNLTnb n | n <- [1..]] == [1,2,10,74,706,...] == A000698 (offset 0)
-- [length $ nub $ map lams2arcsLR $ allcNLTex n | n <- [1..]] == [1,2,6,24,120,720,...] == A000142 (offset 1)
-- [length $ nub $ map lams2arcsLR $ allcNLTexnb n | n <- [1..]] == [1,1,3,13,71,461,...] == A003319 (offset 0)


tamari_map :: [(Int,C.Tree)] -> (Int,C.Tree) -> C.Tree -> StateT Int Maybe (Int,M.OMap)
tamari_map g (x,C.B t1 t2) u = do
  x' <- freshInt
  y <- freshInt
  z <- freshInt
  (r,m) <- tamari_map ((z,t2):g) (y,t1) u
  return
    (r,M.OMap { M.odarts = [x,x'] ++ M.odarts m,
                M.sigma = perm3 x' y z ++ M.sigma m,
                M.alpha = perm2 x x' ++ M.alpha m })
tamari_map g (x,C.L) C.L =
  if g == [] then do
    return (x,M.OMap { M.odarts = [x], M.sigma = [], M.alpha = [] })
  else lift Nothing
tamari_map g (x,C.L) (C.B u1 u2) = 
  let k = C.leaves u1 in
  let grab k g acc =
        if k == 0 then Just (acc,g)
        else if g == [] then Nothing
        else
          let ((x,t):g') = g in
          let i = C.leaves t in
          if i > k then Nothing
          else grab (k - i) g' ((x,t):acc) in
  case grab (k-1) g [] of
    Nothing -> lift Nothing
    Just (g1,(y,t2):g2) -> do
      (r1,m1) <- tamari_map (reverse g1) (x,C.L) u1
      (r2,m2) <- tamari_map g2 (y,t2) u2
      r1' <- freshInt
      r2' <- freshInt
      r <- freshInt
      return (r, M.OMap { M.odarts = [r,r1',r2'] ++ M.odarts m1 ++ M.odarts m2,
                          M.sigma = perm3 r r2' r1' ++ M.sigma m1 ++ M.sigma m2,
                          M.alpha = perm2 r1 r1' ++ perm2 r2 r2' ++
                                    [(i,act (M.alpha m1) i) | i <- M.odarts m1 \\ [r1]] ++
                                    [(i,act (M.alpha m2) i) | i <- M.odarts m2 \\ [r2]] })

tamari_map' :: [(Int,C.Tree)] -> (Int,C.Tree) -> C.Tree -> Maybe (Int,M.OMap)
tamari_map' g (x,t) u =
  let i = 1 + foldr (\(y,t) x -> max y x) x g in
  maybe Nothing (Just . fst) $ runStateT (tamari_map g (x,t) u) i

tamari_map'' :: C.Tree -> C.Tree -> Maybe M.OMap
tamari_map'' t u =
  let c = do
        (r,m) <- tamari_map [] (0,t) u
        d1 <- freshInt
        return $ M.OMap { M.odarts = [d1] ++ M.odarts m,
                          M.sigma =
                            [(i,act (M.sigma m) i) | i <- M.odarts m \\ [0]] ++ perm2 0 d1,
                          M.alpha =
                            [(i,act (M.sigma m) i) | i <- M.odarts m \\ [r]] ++ perm2 d1 r } in
  maybe Nothing (Just . fst) $ runStateT c 1

