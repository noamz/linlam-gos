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
          (diagDyckArcs (cat2dyck t1) === diagCatTree t1 ['1']),
          bigarrow,
          (diagDyckArcs (cat2dyck t2) === diagCatTree t2 ['2']),
          bigarrow,
          (diagDyckArcs (cat2dyck t3) === diagCatTree t3 ['3'])
          ]
  in
  mainWith d
