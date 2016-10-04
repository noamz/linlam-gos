module ScratchPad where

import Data.List

import Lambda
import Maps

vartps :: ULT -> TCtx
vartps t =
  let (_,t') = unlambdas t in
  let (_,_,g,_) = synth t' in
  g

termsig :: ULT -> [Int]
termsig t =
  map length $ map (\(x,tp) -> linearizeType tp) (vartps t)

-- verified for 1<=n<=8
test n =
  let ts = allcNLTex n in
  let ms = map snd (genROM_tutte' n)
  in
   (sort $ map sort $ map termsig ts) ==
   (sort $ map ((sort . map length) . verticesOM) ms)
