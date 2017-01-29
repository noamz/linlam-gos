import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Bijections
import Catalan
import Viz.List

main =
--  let good n f = all (\i -> if i < act f i && i+1 < act f (i+1) then act f i > act f (i+1) else True) [1..2*n-1] in
  let good n f = isConnectedInv f && not (hasKYForbidden f) in
  mainWith $
  vsep 1 $ [hsep 3 $ numberedHList [diagArcs' w # centerX | f <- involute [1..2*n], good n f, let w = inv2arcs f] | n <- [1..4]]
