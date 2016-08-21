{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Catalan

bigarrow = arrowAt (0 ^& 0) unitX
  
main =
  let t1 = B (B L (B (B L L) L)) L in
  let t2 = B L (B (B (B L L) L) L) in
  let t3 = B L (B (B L L) (B L L)) in
  let d =
        diagCatTree t1 ['1'] # centerY |||
        bigarrow |||
        diagCatTree t2 ['2'] # centerY |||
        bigarrow |||
        diagCatTree t3 ['3'] # centerY
  in
  mainWith d
