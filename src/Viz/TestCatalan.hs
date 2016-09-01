{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Catalan
import Viz.List

main =
  mainWith $
  vsep 1 $ [hsep 3 $ numberedHList [vsep 1 [diagTree c # scale 2 # centerX, diagArcs (tree2arcs c) # centerX] | c <- binary_trees n] | n <- [1..4]]
