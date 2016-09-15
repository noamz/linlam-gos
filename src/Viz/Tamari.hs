{-# LANGUAGE NoMonomorphismRestriction #-}

module Viz.Tamari where

import Data.Maybe
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


import Viz.Catalan
import Viz.List
import Viz.Lambda

import Data.List
import Data.Ord

import Bijections
import Catalan
import Tamari

main = do
  putStr "n: "
  n <- getLine >>= return . read
  putStrLn "generating svg..."
  let down_ordered t =
        sortBy (\t1 t2 -> if t1 == t2 then EQ else if tamari_order t1 t2 then LT else if tamari_order t2 t1 then GT else compare t1 t2) $ tamari_down t
  let downsets = map (\t -> (t, down_ordered t)) (binary_trees n)
  let downsets' = sortBy (comparing (length . snd)) $ downsets
  let (ldown,rdown) = unzip downsets'
  let numbered = reverse $ snd $ foldl (\(n,tnss) ts -> let (m,tns) = foldl (\(m,tns) t -> (m+1,(t,m):tns)) (n,[]) ts in (m, (reverse tns) : tnss)) (1,[]) rdown
  let numbered' = zip ldown numbered
  let d = vsep 1 $ labelledVList [(diagTree c # scale 2,
                                   hsep 3 $ labelledHList $ map (\(t,n) -> (diagTree t, text (show n))) tns) | (c,tns) <- numbered' ]

  mainWith (d # frame 1)
