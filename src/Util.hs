module Util where

import Control.Monad
import Control.Monad.State
import Control.Monad.List

-- a generic counter
freshInt :: Monad m => StateT Int m Int
freshInt = do
  x <- get
  () <- put (x+1)
  return x

freshInts :: Monad m => Integer -> StateT Int m [Int]
freshInts 0 = return []
freshInts k = do
  x <- freshInt
  xs <- freshInts (k-1)
  return (x:xs)
