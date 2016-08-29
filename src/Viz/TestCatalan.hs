{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Catalan
import Viz.List

main =
  mainWith $
  vsep 1 $ [hsep 3 $ numberedHList [vsep 1 [diagCatTree c # scale 2 # centerX, diagDyckArcs (cat2dyck c) # centerX] | c <- catalan n] | n <- [1..4]]
