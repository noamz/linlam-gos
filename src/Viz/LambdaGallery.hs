{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Viz.List
import Viz.Lambda

import Data.List
import Data.List.Split (chunksOf)
import Data.Ord

import qualified Lambda as Lam

main = do
  putStr "n: "
  n <- getLine >>= return . read
  putStrLn "generating svg..."
  let b = False
  let ts = Lam.allcULT n
  let root = floor $ sqrt (fromInteger $ toInteger $ length ts)
  let mkDiagram t = vsep 1 [diagULT b t # centerX, text (Lam.prettyULT t) # centerX ]
  let tns = zip ts [1..]
  let ds = map (\(t,n) -> (mkDiagram t, text (show n))) tns
  let d =
        if length ts > 10 then
          vsep 1 $ [hsep 3 $ labelledHList row | row <- chunksOf root ds]
        else hsep 3 $ labelledHList ds
  mainWith (d # frame 1)

