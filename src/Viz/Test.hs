{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Catalan

bigarrow = arrowAt (0 ^& 0) (2*unitX)
  
main =
  let t1 = B (B L (B (B L L) L)) L in
  let t2 = B L (B (B (B L L) L) L) in
  let t3 = B L (B (B L L) (B L L)) in
  let d =
        hsep 1 [
          vsep 1 [diagArcs (tree2arcs t1) # centerX, diagTree t1 # centerX],
          vsep 1 [mempty,bigarrow],
          vsep 1 [diagArcs (tree2arcs t2) # centerX, diagTree t2 # centerX],
          vsep 1 [mempty,bigarrow],
          vsep 1 [diagArcs (tree2arcs t3) # centerX, diagTree t3 # centerX]
          ]
  in
  mainWith d
