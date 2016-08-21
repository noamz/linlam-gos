{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Viz.List where

import Data.Maybe
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

numberList :: [Diagram B] -> [Diagram B]
numberList xs = map (\(x,i) -> hsep 0.5 [alignB (fromOffsets [unit_Y,unitX] ||| text (show i)) & frame 1, alignB x]) (zip xs [1..])
