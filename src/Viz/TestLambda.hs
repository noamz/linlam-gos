{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Viz.List
import Viz.Lambda

import Data.List
import Data.Ord

import Bijections
import qualified Tamari as T
import qualified Lambda as Lam
import qualified Chapo as Ch

main = do
  putStr "(n,b): "
  (n,b) <- getLine >>= return . read
  putStrLn "generating svg..."
  let nptis = Ch.allnpti b n
  let byApps = equivClassesBy (\t1 t2 -> Ch.apps2tree t1 == Ch.apps2tree t2) nptis []
  let byApps' = sortBy (comparing length) $ map (sortBy (\t1 t2 -> let (c1,c2) = (Ch.apps2tree t1,Ch.apps2tree t2) in if c1 == c2 then EQ else if T.tamari_order c1 c2 then LT else if T.tamari_order c2 c1 then GT else compare (Lam.prettyULT t1) (Lam.prettyULT t2))) byApps
  let numbered = reverse $ snd $ foldl (\(n,tnss) ts -> let (m,tns) = foldl (\(m,tns) t -> (m+1,(t,m):tns)) (n,[]) ts in (m, (reverse tns) : tnss)) (1,[]) byApps'
  let numbered' = map (\tns -> (Ch.apps2tree (fst $ head tns),tns)) numbered
  let mkDiagram t = vsep 1 [diagULT b t # centerX, text (Lam.prettyULT t) # centerX ]
  let d = vsep 1 $ labelledVList [(diagTree c # scale 2,
                                   hsep 3 $ labelledHList $ map (\(t,n) -> (mkDiagram t, text (show n))) tns) | (c,tns) <- numbered' ]

  mainWith (d # frame 1)
