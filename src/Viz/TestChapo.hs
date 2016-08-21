{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Viz.List

import Data.List
import Data.Ord

import Bijections
import qualified Lambda as Lam
import qualified Chapo as Ch
import qualified Tamari as T

bigarrow = arrowAt (0 ^& 0) (2*unitX)
  
main =
  let npti3 = Ch.allnptiLR 3 in
  let byApps = equivClassesBy (\t1 t2 -> Ch.apps2cat t1 == Ch.apps2cat t2) npti3 [] in
  let ts = concat $ sortBy (comparing length) $ map (sortBy (\t1 t2 -> let (c1,c2) = (Ch.apps2cat t1,Ch.apps2cat t2) in if c1 == c2 then EQ else if T.tamari_order c1 c2 then LT else if T.tamari_order c2 c1 then GT else compare (Lam.prettyULT t1) (Lam.prettyULT t2))) byApps in
  let tminterval t = (Ch.lams2dowLR t,Ch.apps2cat t) in
  let intdiag (w,c) = vsep 1 [diagDyckArcs w, diagCatTree [] c] in
  let d = hsep 1 $ numberList $ map (\t -> vsep 1 [intdiag (tminterval t) # centerX, text (Lam.prettyULT t) # centerX ]) ts
  in
  mainWith (d # frame 1)
