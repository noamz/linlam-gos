-- decision procedures for Lambek calculus

module Lambek where

import Data.List
import Bijections
import Catalan
import Control.Monad.List

data Formula = Atm Int | LImp Formula Formula | RImp Formula Formula
  deriving (Show,Eq)

genFormula :: [Int] -> [Formula]
genFormula [] = mzero
genFormula [x] = return (Atm x)
genFormula g = do
  (g1,g2) <- splitCN g
  a <- genFormula g1
  b <- genFormula g2
  return (LImp a b) `mplus` return (RImp a b)

subFormula :: Formula -> [Formula]
subFormula (LImp a b) = a : b : subFormula a ++ subFormula b
subFormula (RImp a b) = a : b : subFormula a ++ subFormula b
subFormula (Atm x) = []

posFormula :: Formula -> [Formula]
negFormula :: Formula -> [Formula]
posFormula (LImp a b) = b : negFormula a ++ posFormula b
posFormula (RImp a b) = b : negFormula a ++ posFormula b
posFormula (Atm x) = []
negFormula (LImp a b) = a : posFormula a ++ negFormula b
negFormula (RImp a b) = a : posFormula a ++ negFormula b
negFormula (Atm x) = []

goodFormula :: Bool -> Formula -> Bool
goodFormula _ (Atm _) = True
goodFormula True (LImp a b) = goodFormula False a && goodFormula True b
goodFormula True (RImp a b) = False
goodFormula False (LImp a b) = False
goodFormula False (RImp a b) = goodFormula True a && goodFormula False b

countL :: Formula -> Int
countL (Atm _) = 0
countL (LImp a b) = 1 + countL a + countL b
countL (RImp a b) = countL a + countL b

countR :: Formula -> Int
countR (Atm _) = 0
countR (LImp a b) = countR a + countR b
countR (RImp a b) = 1 + countR a + countR b

lambek_rinv :: [Formula] -> Formula -> Bool
lambek_neu :: [Formula] -> Int -> Bool
lambek_lfoc :: [Formula] -> Formula -> [Formula] -> Int -> Bool

lambek_rinv g (LImp a b) = lambek_rinv (a : g) b
lambek_rinv g (RImp a b) = lambek_rinv (g ++ [a]) b
lambek_rinv g (Atm x) = lambek_neu g x

lambek_neu g x =
  flip any (splitC g) $ \(g1,g2) ->
    case g2 of
      (a:g2') -> lambek_lfoc g1 a g2' x
      [] -> False
      
lambek_lfoc g1 (Atm y) g2 x = g1 == [] && y == x && g2 == []
lambek_lfoc g1 (LImp a b) g2 x =
  flip any (splitC g1) $ \(g11,g12) -> lambek_rinv g12 a && lambek_lfoc g11 b g2 x
lambek_lfoc g1 (RImp a b) g2 x =
  flip any (splitC g2) $ \(g21,g22) -> lambek_lfoc g1 b g22 x && lambek_rinv g21 a

-- [length $ [a | vars <- map (arcs2dow . tree2arcs) (binary_trees n), a <- genFormula vars, goodFormula True a, lambek_rinv [] a] | n <- [1..]] == [1,2,9,54,378,...]

-- [length $ [a | vars <- map (arcs2dow . tree2arcs) (binary_trees n), a <- genFormula vars, goodFormula True a, lambek_rinv [] a, flip all (subFormula a) (\b -> not (lambek_rinv [] b))] | n <- [1..6]] == [1,1,3,13,...]
