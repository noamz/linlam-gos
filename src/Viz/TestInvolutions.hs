import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Bijections
import Catalan
import Viz.List

main =
  mainWith $
  vsep 1 $ [hsep 3 $ numberedHList [diagArcs w # centerX | f <- involute [1..2*n], let w = inv2arcs f] | n <- [1..3]]
