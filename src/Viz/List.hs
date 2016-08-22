{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Viz.List where

import Data.Maybe
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

labelledHList :: [(Diagram B,Diagram B)] -> [Diagram B]
labelledHList xis = map (\(x,i) -> hsep 3 [alignB (fromOffsets [unit_Y,unitX] # lwL 0.5 ||| i) & frame 1, alignB x]) xis

labelledVList :: [(Diagram B,Diagram B)] -> [Diagram B]
labelledVList xis = map (\(x,i) -> hsep 0.5 [alignT (fromOffsets [unitY,unitX] # lwL 0.5 === i) & frame 1, alignT x]) xis

numberedHList :: [Diagram B] -> [Diagram B]
numberedHList xs = labelledHList $ zip xs (map (text . show) [1..])

numberedVList :: [Diagram B] -> [Diagram B]
numberedVList xs = labelledVList $ zip xs (map (text . show) [1..])
