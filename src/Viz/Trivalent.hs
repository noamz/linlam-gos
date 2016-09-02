{-# LANGUAGE NoMonomorphismRestriction #-}

module Viz.Trivalent where


import Data.Maybe
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Catalan as C

import qualified Viz.Catalan as VC

diagTrivalent :: Tree -> Arcs -> Diagram B
diagTrivalent c w =
  let (d,ns) = VC.diagTree_st [] c in
  VC.diagArcs_glue ns w d


