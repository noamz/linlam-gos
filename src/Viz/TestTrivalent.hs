{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Viz.Catalan
import Viz.List
import Viz.Trivalent

import Data.List
import Data.Ord

import Bijections
import qualified Tamari as T
import qualified Lambda as Lam
import qualified Chapo as Ch

main = do
  putStr "n: "
  n <- getLine >>= return . read
  putStrLn "generating svg..."
  let b = False
  let npts = Lam.allcNPT b n
  let d = hsep 3 $ numberedHList [diagTrivalent (Lam.skel2tree b $ Lam.lambdaSkel t) (Ch.lams2arcs b t) # centerX | t <- npts]
  mainWith (d # frame 1)
