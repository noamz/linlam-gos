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
  let withTopChords = map (\f -> (length (l2rmaximalChords f), f)) invs
  let byTopChords = equivClassesBy (\(k1,f1) (k2,f2) -> k1 == k2) withTopChords []
  let byTopChords' = sortBy (comparing (fst . head)) byTopChords
  let d = vsep 1 $ labelledVList [(text ("k = " ++  show k),
                                  hsep 3 $ numberedHList [diagArcs' w # centerX | f <- fs, let w = inv2arcs f]) | bytc <- byTopChords', let k = fst (head bytc), let fs = map snd bytc]
  mainWith (d # frame 1)
  

