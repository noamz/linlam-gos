{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Viz.List
import qualified Lambda as Lam

bigarrow = arrowAt (0 ^& 0) (2*unitX)
  
main =
  let lam3 = Lam.allcNPT False 3 in
  let d = hsep 1 $ numberList $ map (\t -> (diagCatTree (Lam.lambdaSkel False t) [] # centerX === text (Lam.prettyULT t) # centerX) # centerY) lam3
  in
  mainWith (d # frame 1)
