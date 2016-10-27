module ScratchPad where

import Data.List

import Formulas
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

-- verified for 1<=n<=9
test2 n =
  let ts = allcNPTnb True n in
  let a000139' n = if n == 0 then 1 else
                     2*fact (3*n) `div` (fact (2*n+1) * fact(n+1)) in
  let nsts =
        [t | t <- ts, flip all (t:subnormal_subnormals t) $ \t' ->
          let (xs,u) = unlambdas t' in
          length xs <= 1 + length (snd (unapps u []))] in
  toInteger (length nsts) == a000139' (toInteger (n-1))

