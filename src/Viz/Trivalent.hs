{-# LANGUAGE NoMonomorphismRestriction #-}

module Viz.Trivalent where


import Data.Maybe
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Catalan as C

import qualified Viz.Catalan as VC

diagTrivalent :: Catalan -> [Int] -> Diagram B
diagTrivalent c w =
  let (d,ns) = VC.catTree [] c in
  VC.diagDyckArcs_glue ns w d


