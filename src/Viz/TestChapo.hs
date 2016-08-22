{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Viz.List

import Data.List
import Data.Ord

import Bijections
import qualified Lambda as Lam
import qualified Chapo as Ch
import qualified Tamari as T

bigarrow = arrowAt (0 ^& 0) (2*unitX)
  
main = do
  putStr "n: "
  n <- getLine >>= return . read
  putStrLn "generating svg..."
  let nptis = Ch.allnptiLR n
  let byApps = equivClassesBy (\t1 t2 -> Ch.apps2cat t1 == Ch.apps2cat t2) nptis []
  let byApps' = sortBy (comparing length) $ map (sortBy (\t1 t2 -> let (c1,c2) = (Ch.apps2cat t1,Ch.apps2cat t2) in if c1 == c2 then EQ else if T.tamari_order c1 c2 then LT else if T.tamari_order c2 c1 then GT else compare (Lam.prettyULT t1) (Lam.prettyULT t2))) byApps
  let byApps'' = map (\ts -> (Ch.apps2cat (head ts),ts)) byApps'
  
  let tminterval t = (Ch.lams2dowLR t,Ch.apps2cat t)
  let d = vsep 1 $ labelledVList [(diagCatTree [] c # scale 2,
                                   hsep 1 $ labelledHList $ zip (map (\t -> vsep 1 [diagDyckArcs (Ch.lams2dowLR t) # centerX, text (Lam.prettyULT t) # centerX ]) ts) (repeat mempty)) | (c,ts) <- byApps'']
  mainWith (d # frame 1)
