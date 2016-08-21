{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Viz.List
import qualified Lambda as Lam
import qualified Chapo as Ch

bigarrow = arrowAt (0 ^& 0) (2*unitX)
  
main =
  let npti3 = Ch.allnptiLR 3 in
  let tminterval t = (Ch.lams2dowLR t,Ch.apps2cat t) in
  let intdiag (w,c) = vsep 1 [diagDyckArcs w, diagCatTree [] c] in
  let d = hsep 1 $ numberList $ map (\t -> vsep 1 [intdiag (tminterval t) # centerX, text (Lam.prettyULT t) # centerX ]) npti3
  in
  mainWith (d # frame 1)
