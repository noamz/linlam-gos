{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.GraphViz

import           Data.GraphViz
import           Data.GraphViz.Commands

import Maps
import LamTri

gr =
--  let (vs,es) = graphOM (cmap $ rootedTutteGraph) in
  let (vs,es) = graphOM (deroot $ dodecaRTM) in
  mkGraph vs (map (\(i,j) -> (i,j,())) es)

graphvizExample1 = do
  gr' <- layoutGraph Neato gr
  let grDrawing :: Diagram B
      grDrawing = drawGraph
                     (const $ place (circle 5 # fc black))
                     (\_ p1 _ p2 _ p -> arrowBetween' (opts p) p1 p2 # lwL 1)
                     gr'
      opts p = with & gaps .~ 16 & arrowHead .~ noHead
  return (grDrawing # frame 1)

main = graphvizExample1 >>= defaultMain
  where
    theGraph :: IO (Diagram B)
    theGraph = simpleGraphDiagram Dot gr

