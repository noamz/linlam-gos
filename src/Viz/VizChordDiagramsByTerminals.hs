import Data.List
import Data.Ord

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Bijections
import Catalan
import ChordDiagrams
import Viz.List

main = do
  putStr "(n, isConnected): "
  (n,b) <- getLine >>= return . read
  let invs = filter (if b then isConnectedInv else isIndecompInv) (involute [1..2*n])
  let withTermChords = map (\f -> (length (terminalChords f), f)) invs
  let byTermChords = equivClassesBy (\(k1,f1) (k2,f2) -> k1 == k2) withTermChords []
  let byTermChords' = sortBy (comparing (fst . head)) byTermChords
  let d = vsep 1 $ labelledVList [(text ("k = " ++  show k),
                                  hsep 3 $ numberedHList [diagArcs' w # centerX | f <- fs, let w = inv2arcs f]) | bytc <- byTermChords', let k = fst (head bytc), let fs = map snd bytc]
  mainWith (d # frame 1)
  

