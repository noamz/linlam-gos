-- some code related to the bijection in:
--   "Connected chord diagrams and bridgeless maps"
--   Julien Courtiel, Karen Yeats, Noam Zeilberger
--   https://arxiv.org/abs/1611.04611

module MapsCDs where

import Data.List
import Data.Maybe
import Control.Monad.State

import Util
import Bijections
import Catalan
import ChordDiagrams
import Maps

data DTree = DT0 | DT1 Int DTree | DT2 DTree DTree
  deriving (Show,Eq)

dt2arcs_st :: DTree -> State Int Arcs
dt2arcs_st DT0 = do
  n <- freshInt
  return [U n,D n]
dt2arcs_st (DT1 i t) = do
  n <- freshInt
  p <- dt2arcs_st t
  let (pre,post) = splitAt i p
  if i > 0 && i <= length p then
    return (U n : pre ++ D n : post)
  else
    error "must have 0 < i <= length"
dt2arcs_st (DT2 t1 t2) = do
  n <- freshInt
  p1 <- dt2arcs_st t1
  p2 <- dt2arcs_st t2
  return (U n : p1 ++ D n : p2)

dt2arcs :: DTree -> Arcs
dt2arcs t = fst $ runState (dt2arcs_st t) 0

arcs2dt :: Arcs -> DTree
arcs2dt [] = error "empty diagram"
arcs2dt (U x:p) =
  let i = fromJust (findIndex (==D x) p) in
  let (p1,p2) = decompArcs [] p in
  if p1 == [D x] && null p2 then DT0 else
  if null p2 then DT1 i (arcs2dt (p1 \\ [D x]))
  else DT2 (arcs2dt p1) (arcs2dt (U x:p2))
arcs2dt p = error ("not a closed arc diagram" ++ show p)

dt2rom_st :: DTree -> State Int OMap
dt2rom_st DT0 = do
  r <- freshInt
  return $ OMap {odarts=[r],sigma=[(r,r)],alpha=[(r,r)]}
dt2rom_st (DT2 t1 t2) = do
  m1 <- dt2rom_st t1
  m2 <- dt2rom_st t2
  let r1 = head (odarts m1)
  let r2 = head (odarts m2)
  r <- freshInt
  let odarts' = r : odarts m1 ++ odarts m2
  let sigma' = [(r,r2)] ++
               [(d,if d' == r2 then r else d') | d <- odarts m2, let d' = act (sigma m2) d] ++
               sigma m1
  let alpha' = [(r,r)] ++
               [(r1,r2),(r2,r1)] ++
               [(d,d') | d <- odarts m1 \\ [r1], let d' = act (alpha m1) d] ++
               [(d,d') | d <- odarts m2 \\ [r2], let d' = act (alpha m2) d]
  return $ OMap { odarts = odarts', sigma = sigma', alpha = alpha' }
dt2rom_st (DT1 i t) = do
  m1 <- dt2rom_st t
  let r1 = head (odarts m1)
  r2 <- freshInt
  r <- freshInt
  let (pre,x:post) = splitAt (i-1) (odarts m1) 
  let odarts' = r : pre ++ x : r2 : post
  let r2' = if act (sigma m1) x == r1 then r else act (sigma m1) x
  let sigma' = [(r,r1)] ++ [(x,r2),(r2,r2')] ++
               [(d,if d' == r1 then r else d') | d <- odarts m1 \\ [x], let d' = act (sigma m1) d]
  let alpha' = [(r,r)] ++
               [(r1,r2),(r2,r1)] ++
               [(d,d') | d <- odarts m1 \\ [r1], let d' = act (alpha m1) d]
  return $ OMap { odarts = odarts', sigma = sigma', alpha = alpha' }

dt2rom :: DTree -> OMap
dt2rom t = fst $ runState (dt2rom_st t) 0

arcs2rom :: Arcs -> OMap
arcs2rom = dt2rom . arcs2dt
