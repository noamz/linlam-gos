{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Catalan
import Viz.List

bigarrow = arrowAt (0 ^& 0) (2*unitX)
  
main =
  let cat4 = catalan 4 in
  let d = hsep 1 $ numberedHList $ map (\t -> diagCatTree [] t) cat4
  in
  mainWith d
