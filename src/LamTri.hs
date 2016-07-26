-- converting between linear lambda terms and rooted trivalent maps

module LamTri where

import Control.Monad
import Control.Monad.State
import Control.Monad.List
import Data.IORef
import Data.Maybe
import Data.List

import Util
import Formulas
import Bijections
import qualified Maps as M
import Lambda

-- type of rooted trivalent maps with boundary
type RTM = (M.OMap,[Int],Int)
-- type of trivalent maps with boundary
type TMb = (M.OMap,[Int])

cmap :: RTM -> M.OMap
cmap (m,ds,d0) = m

boundary :: RTM -> ([Int],Int)
boundary (m,ds,d0) = (ds,d0)

-- Inductive procedure for converting linear lambda terms to rooted
-- trivalent maps with boundary.  This runs in the State monad, in
-- order to generate fresh darts.
toRTM :: ULT -> State Int RTM
toRTM (V x) = return $ (M.OMap { M.odarts = [x], M.sigma = [(x,x)], M.alpha = [(x,x)] }, [x], x)
toRTM (A t u) = do
  (m1,g1,r1) <- toRTM t
  (m2,g2,r2) <- toRTM u
  r <- freshInt
  r' <- freshInt
  return $ (M.OMap { M.odarts = [r,r'] ++ M.odarts m1 ++ M.odarts m2,
                     M.sigma = [(r,r)] ++ perm3 r' r2 r1 ++
                               [(i,act (M.sigma m1) i) | i <- M.odarts m1 \\ [r1]] ++
                               [(i,act (M.sigma m2) i) | i <- M.odarts m2 \\ [r2]],
                     M.alpha = perm2 r r' ++ M.alpha m1 ++ M.alpha m2},
            g1 ++ g2,r)
toRTM (L x t) = do
  (m1,g1,r1) <- toRTM t
  r <- freshInt
  r' <- freshInt
  x' <- freshInt
  let g1' = if not (elem x g1) then error (show x ++ " not in " ++ show g1)
            else delete x g1
  return $ (M.OMap { M.odarts = [r,r',x'] ++ M.odarts m1,
                     M.sigma = [(r,r)] ++ perm3 r' x' r1 ++
                               [(i,act (M.sigma m1) i) | i <- M.odarts m1 \\ [r1]],
                     M.alpha = perm2 x x' ++ perm2 r r' ++
                               [(i,act (M.alpha m1) i) | i <- M.odarts m1 \\ [x]]},
            g1',r)

maxVar :: ULT -> Int -> Int
maxVar (V x) d = if x > d then x else d
maxVar (A t1 t2) d = maxVar t2 (maxVar t1 d)
maxVar (L x t) d = maxVar t (if x > d then x else d)

toRTM' t =
  let d = maxVar t (-1) in
  fst $ runState (toRTM t) (d+1)

-- Inductive procedure for converting trivalent maps with boundary into linear
-- lambda terms.  The free variables of the constructed term correspond
-- to the boundary of the original map.
toULTc :: RTM -> ULTc
toULTc (m,ds,d0) = (ds,toULT' m d0)
  where
    toULT' :: M.OMap -> Int -> ULT
    toULT' m d0 =
      if act (M.alpha m) d0 == d0 then V d0
      else
        let d0' = act (M.alpha m) d0 in
        let d2 = act (M.sigma m) d0' in
        let d1 = act (M.sigma m) d2 in
        -- test if d1 and d2 remain connected after removing the trivalent vertex
        let m' = m { M.sigma = [(d1,d1),(d2,d2)] ++ [(i,act (M.sigma m) i) | i <- M.odarts m \\ [d1,d2]] } in
        let closure = transClosure [M.sigma m', M.alpha m'] [d1] in
        if elem d2 closure then
          let x = act (M.alpha m) d2 in
          L x (toULT' (m' { M.alpha = [(x,x)] ++ [(i,act (M.alpha m') i) | i <- M.odarts m' \\ [x]] }) d1)
        else
          A (toULT' m' d1) (toULT' m' d2)

-- compute the euler characteristic of a linear lambda term
eulerULT :: ULT -> Int
eulerULT t = let (m,_,_) = toRTM' t in M.eulerOM m

-- all closed linear lambda terms, indexed by n, paired with their characteristic
allTerms :: [(Integer,[(ULT,Int)])]
allTerms = [(n, [(t,eulerULT t) | t <- allcULT (2*n+1)]) | n <- [0..]]

-- all closed indecomposable linear lambda terms, indexed by n, paired with their characteristic
allNBTerms :: [(Integer,[(ULT,Int)])]
allNBTerms = [(n, [(t,eulerULT t) | t <- allcULTnb (2*n+1)]) | n <- [0..]]

-- all closed indecomposable planar terms
allPlaInd :: [ULT]
allPlaInd = [t | n <- [0..], t <- allcULTnb (2*n+1), eulerULT t == 2]

allNormalPlaInd :: [(Integer,[ULT])]
allNormalPlaInd = [(n, [t | t <- allcULTnb (2*n+1), eulerULT t == 2, isNormal t]) | n <- [0..]]


a000260 = [length [t | t <- allcULTnb (2*n+1), isNormal t, isPlanar t] | n <- [0..]]
--- a000260 = [1,1,3,13,68,...]

a000309 = [length [t | t <- allcULTnb (2*n+1), isPlanar t] | n <- [0..]]
--- a000309 = [1,1,4,24,176,...]

printTerms :: [(Integer,[(ULT,Int)])] -> IO ()
printTerms ts = do
  mapM (\ (n,l) -> putStr ("n=" ++ show n ++ ":\n") >> mapM (\ (t,_) -> printULT t) l) ts
  return ()

flattenTerms :: [(Integer,[(ULT,Int)])] -> [ULT]
flattenTerms ts = concatMap (\ (_, l) -> map fst l) ts

byChi :: Int -> [(Integer,[(ULT,Int)])] -> [(Integer,[(ULT,Int)])]
byChi chi = map (\ (n,l) -> (n, filter (\ (t,c) -> c == chi) l))

byGenus g = byChi (2 - 2*g)

allPlanar = byGenus 0 allTerms
allToric = byGenus 1 allTerms

{- Example interaction:

> printTerms (take 2 allPlanar)
n=0:
\x -> x
n=1:
(\x -> x)(\y -> y)
\x -> x(\y -> y)
\x -> (\y -> y)(x)
\x -> \y -> x(y)

> printTerms (take 3 allToric)
n=0:
n=1:
\x -> \y -> y(x)
(\x -> x)(\y -> \z -> z(y))
(\x -> \y -> y(x))(\z -> z)
\x -> x(\y -> \z -> z(y))
\x -> (\y -> y(x))(\z -> z)
\x -> (\y -> y)(\z -> z(x))
\x -> (\y -> \z -> z(y))(x)
\x -> \y -> y(x)(\z -> z)
\x -> \y -> x(\z -> z(y))
\x -> \y -> (\z -> z(x))(y)
\x -> \y -> y(x(\z -> z))
\x -> \y -> y((\z -> z)(x))
\x -> \y -> y(\z -> x(z))
\x -> \y -> y(\z -> z(x))
\x -> \y -> y(\z -> z)(x)
\x -> \y -> (\z -> z)(y)(x)
\x -> \y -> (\z -> y(z))(x)
\x -> \y -> (\z -> z(y))(x)
\x -> \y -> (\z -> z)(y(x))
\x -> \y -> \z -> y(x)(z)
\x -> \y -> \z -> x(z)(y)
\x -> \y -> \z -> z(x)(y)
\x -> \y -> \z -> x(z(y))
\x -> \y -> \z -> y(z)(x)
\x -> \y -> \z -> z(y)(x)
\x -> \y -> \z -> y(x(z))
\x -> \y -> \z -> y(z(x))
\x -> \y -> \z -> z(x(y))
\x -> \y -> \z -> z(y(x))

-}

-- NOTE: [Vidal 2010] counts rooted triangular maps by genus in:
--  Samuel Alexandre Vidal. An optimal algorithm to generate rooted trivalent diagrams and rooted triangular maps.  Theoretical Computer Science 411 (2010).
-- See in particular Tables 12 & 14 of http://cafemath.fr/articles/diaggen.pdf

lengths :: [(t,[a])] -> [Int]
lengths = map (\ (_,l) -> length l)

a062980 = lengths allTerms
-- a062980 == [1,5,60,1105,27120,...]

a002005 = lengths allPlanar
-- a002005 == [1,4,32,336,4096,...]

-- lengths allToric == [0,1,28,664,14912,...]
-- NOTE: not in OEIS but agrees with row g=1 of Table 14 in [Vidal 2010]

-- lengths (byGenus 2 allTerms) == [0,0,0,105,8112,...]
-- NOTE: not in OEIS but agrees with row g=2 of Table 14 in [Vidal 2010]

isPlanar t = eulerULT t == 2

allNormal = map (\ (n,l) -> (n, filter (\ (t,c) -> isNormal t) l)) allTerms
-- lengths allNormal == [1,3,26,367,7142,...]

allPlanarNormal = map (\ (n,l) -> (n, filter (\ (t,c) -> isNormal t) l)) allPlanar
allNormalPlanar = map (\ (n,l) -> (n, filter (\ (t,c) -> c == 2) l)) allNormal
-- note: allNormalPlanar seems to generate faster than allPlanarNormal

a000168 = map (\ (n,l) -> length l) allNormalPlanar
-- a000168 == [1,2,9,54,378,2916,...]

allToricNormal = map (\ (n,l) -> (n, filter (\ (t,c) -> isNormal t) l)) allToric
allNormalToric = map (\ (n,l) -> (n, filter (\ (t,c) -> c == 0) l)) allNormal
-- lengths allNormalToric == [0,1,17,244,3294,...]

-- byFaces f =
--   map (\ (n,l) -> (n, filter (\ (t,c) -> length (facesCycles (toRTM' t)) == f) l))

-- all NLTs and NPTs of exactly a given size
allNLTs n = allcNLT n
allNPTs lr n = allcNPT lr n

-- all NLTs and NPTs of at most a given size
allNLTsupto n = flattenTerms (take n allNormal)
allNPTsupto n = flattenTerms (take n allNormalPlanar)

{-
> inferPrintTerms $ allNLTuptos 3
 |- \x -> x : (a -> a)
 |- \x -> x(\y -> y) : (((b -> b) -> a) -> a)
 |- \x -> \y -> x(y) : ((b -> a) -> (b -> a))
 |- \x -> \y -> y(x) : (b -> ((b -> a) -> a))
 |- \x -> x(\y -> y(\z -> z)) : (((((c -> c) -> b) -> b) -> a) -> a)
 |- \x -> x(\y -> \z -> y(z)) : ((((c -> b) -> (c -> b)) -> a) -> a)
 |- \x -> x(\y -> \z -> z(y)) : (((c -> ((c -> b) -> b)) -> a) -> a)
 |- \x -> x(\y -> y)(\z -> z) : (((c -> c) -> ((b -> b) -> a)) -> a)
 |- \x -> \y -> x(y)(\z -> z) : ((c -> ((b -> b) -> a)) -> (c -> a))
 |- \x -> \y -> y(x)(\z -> z) : (c -> ((c -> ((b -> b) -> a)) -> a))
 |- \x -> \y -> x(y(\z -> z)) : ((b -> a) -> (((c -> c) -> b) -> a))
 |- \x -> \y -> x(\z -> y(z)) : (((c -> b) -> a) -> ((c -> b) -> a))
 |- \x -> \y -> x(\z -> z(y)) : ((((c -> b) -> b) -> a) -> (c -> a))
 |- \x -> \y -> x(\z -> z)(y) : (((c -> c) -> (b -> a)) -> (b -> a))
 |- \x -> \y -> y(x(\z -> z)) : (((c -> c) -> b) -> ((b -> a) -> a))
 |- \x -> \y -> y(\z -> x(z)) : ((c -> b) -> (((c -> b) -> a) -> a))
 |- \x -> \y -> y(\z -> z(x)) : (c -> ((((c -> b) -> b) -> a) -> a))
 |- \x -> \y -> y(\z -> z)(x) : (b -> (((c -> c) -> (b -> a)) -> a))
 |- \x -> \y -> \z -> x(y)(z) : ((c -> (b -> a)) -> (c -> (b -> a)))
 |- \x -> \y -> \z -> y(x)(z) : (c -> ((c -> (b -> a)) -> (b -> a)))
 |- \x -> \y -> \z -> x(z)(y) : ((c -> (b -> a)) -> (b -> (c -> a)))
 |- \x -> \y -> \z -> z(x)(y) : (c -> (b -> ((c -> (b -> a)) -> a)))
 |- \x -> \y -> \z -> x(y(z)) : ((b -> a) -> ((c -> b) -> (c -> a)))
 |- \x -> \y -> \z -> x(z(y)) : ((b -> a) -> (c -> ((c -> b) -> a)))
 |- \x -> \y -> \z -> y(z)(x) : (b -> ((c -> (b -> a)) -> (c -> a)))
 |- \x -> \y -> \z -> z(y)(x) : (b -> (c -> ((c -> (b -> a)) -> a)))
 |- \x -> \y -> \z -> y(x(z)) : ((c -> b) -> ((b -> a) -> (c -> a)))
 |- \x -> \y -> \z -> y(z(x)) : (c -> ((b -> a) -> ((c -> b) -> a)))
 |- \x -> \y -> \z -> z(x(y)) : ((c -> b) -> (c -> ((b -> a) -> a)))
 |- \x -> \y -> \z -> z(y(x)) : (c -> ((c -> b) -> ((b -> a) -> a)))
> 
> inferPrintTerms $ map snd (allNeutral 1)
x:((b -> b) -> a) |- x(\y -> y) : a
y:(b -> a),x:b |- y(x) : a
x:(b -> a),y:b |- x(y) : a
> inferPrintTerms $ map snd (allNeutral 2)
x:((((c -> c) -> b) -> b) -> a) |- x(\y -> y(\z -> z)) : a
x:((c -> ((c -> b) -> b)) -> a) |- x(\y -> \z -> z(y)) : a
x:(((c -> b) -> (c -> b)) -> a) |- x(\y -> \z -> y(z)) : a
x:((c -> c) -> ((b -> b) -> a)) |- x(\y -> y)(\z -> z) : a
y:(c -> ((b -> b) -> a)),x:c |- y(x)(\z -> z) : a
x:(c -> ((b -> b) -> a)),y:c |- x(y)(\z -> z) : a
y:(b -> a),x:((c -> c) -> b) |- y(x(\z -> z)) : a
y:(((c -> b) -> b) -> a),x:c |- y(\z -> z(x)) : a
y:((c -> b) -> a),x:(c -> b) |- y(\z -> x(z)) : a
y:((c -> c) -> (b -> a)),x:b |- y(\z -> z)(x) : a
x:(b -> a),y:((c -> c) -> b) |- x(y(\z -> z)) : a
x:(((c -> b) -> b) -> a),y:c |- x(\z -> z(y)) : a
x:((c -> b) -> a),y:(c -> b) |- x(\z -> y(z)) : a
x:((c -> c) -> (b -> a)),y:b |- x(\z -> z)(y) : a
z:(c -> (b -> a)),y:c,x:b |- z(y)(x) : a
y:(c -> (b -> a)),z:c,x:b |- y(z)(x) : a
z:(c -> (b -> a)),x:c,y:b |- z(x)(y) : a
x:(c -> (b -> a)),z:c,y:b |- x(z)(y) : a
z:(b -> a),y:(c -> b),x:c |- z(y(x)) : a
z:(b -> a),x:(c -> b),y:c |- z(x(y)) : a
y:(c -> (b -> a)),x:c,z:b |- y(x)(z) : a
x:(c -> (b -> a)),y:c,z:b |- x(y)(z) : a
y:(b -> a),z:(c -> b),x:c |- y(z(x)) : a
y:(b -> a),x:(c -> b),z:c |- y(x(z)) : a
x:(b -> a),z:(c -> b),y:c |- x(z(y)) : a
x:(b -> a),y:(c -> b),z:c |- x(y(z)) : a
-}

{-
inferPrintTerms $ allNPTuptos 3
 |- \x -> x : (a -> a) 
 |- \x -> x(\y -> y) : (((b -> b) -> a) -> a) 
 |- \x -> \y -> x(y) : ((b -> a) -> (b -> a)) 
 |- \x -> x(\y -> y(\z -> z)) : (((((c -> c) -> b) -> b) -> a) -> a) 
 |- \x -> x(\y -> \z -> y(z)) : ((((c -> b) -> (c -> b)) -> a) -> a) 
 |- \x -> x(\y -> y)(\z -> z) : (((c -> c) -> ((b -> b) -> a)) -> a) 
 |- \x -> \y -> x(y)(\z -> z) : ((c -> ((b -> b) -> a)) -> (c -> a)) 
 |- \x -> \y -> x(y(\z -> z)) : ((b -> a) -> (((c -> c) -> b) -> a)) 
 |- \x -> \y -> x(\z -> y(z)) : (((c -> b) -> a) -> ((c -> b) -> a)) 
 |- \x -> \y -> x(\z -> z)(y) : (((c -> c) -> (b -> a)) -> (b -> a)) 
 |- \x -> \y -> \z -> x(y)(z) : ((c -> (b -> a)) -> (c -> (b -> a))) 
 |- \x -> \y -> \z -> x(y(z)) : ((b -> a) -> ((c -> b) -> (c -> a)))
-}

-- defining classes of terms by properties of their normal form

allNormalizesPlanar = [(n, [t | t <- allcULT (2*n+1), eulerULT (normalize t) == 2]) | n <- [0..]]
-- lengths allNormalizesPlanar == [1,4,35,430,6486,...]


{-- UNROOTING AND RE-ROOTING MAPS WITH BOUNDARY --}

-- turn a rooted trivalent map with boundary into a trivalent map with boundary by forgetting the root vertex.
unroot :: RTM -> TMb
unroot (m,ds,d0) =
  let d0' = act (M.alpha m) d0 in
  (M.OMap { M.odarts = delete d0 (M.odarts m),
            M.sigma = M.sigma m,
            M.alpha = [(d0',d0')] ++ [(i,act (M.alpha m) i) | i <- M.odarts m \\ [d0,d0']] },
   d0':ds)

-- turn a closed rooted trivalent map into an unrooted trivalent map by deleting the root vertex and attached trivalent vertex (four darts in total).
deroot :: RTM -> M.OMap
deroot (m,[],d0) =
  let d0' = act (M.alpha m) d0 in
  let d1 = act (M.sigma m) d0' in
  let d2 = act (M.sigma m) d1 in
  let (d1',d2') = (act (M.alpha m) d1, act (M.alpha m) d2) in
  M.OMap { M.odarts = M.odarts m \\ [d0,d0',d1,d2],
           M.sigma = M.sigma m,
           M.alpha = perm2 d1' d2' ++ [(i,act (M.alpha m) i) | i <- M.odarts m \\ [d0,d0',d1,d2,d1',d2']] }
deroot (m,_,d0) = error "cannot deroot map with boundary"

-- mark a free edge on the boundary of a map
root :: TMb -> Int -> Int -> RTM
root (m,ds) i d0 =
  let (left,right) = splitAt i ds in
  let (di,ds') = (head right, left ++ tail right) in
  (M.OMap { M.odarts = d0 : M.odarts m,
            M.sigma = [(d0,d0)] ++ M.sigma m,
            M.alpha = perm2 di d0 ++ [(i,act (M.alpha m) i) | i <- M.odarts m \\ [di]] },
   ds',
   d0)

-- flip the root (alpha)
flip_root :: RTM -> RTM
flip_root (m,g,r) =
  let x1 = act (M.alpha m) r in
  let x2 = act (M.sigma m) x1 in
  let x3 = act (M.sigma m) x2 in
  (m { M.sigma = [(i,j) | i <- M.odarts m, let j = if i == x1 then x3 else if i == x3 then x2 else if i == x2 then x1 else act (M.sigma m) i ] }, g, r)

flip_ULT :: ULT -> ULT
flip_ULT = snd . toULTc . flip_root . toRTM'

-- turn the root (sigma)
turn_root :: RTM -> RTM
turn_root (m,g,r) =
  let x1 = act (M.alpha m) r in
  let x2 = act (M.sigma m) x1 in
  let x3 = act (M.sigma m) x2 in
  let z1 = act (M.alpha m) x2 in
  let y1 = act (M.alpha m) x3 in
  let y2 = act (M.sigma m) y1 in
  let w1 = act (M.alpha m) y2 in
  (m { M.alpha = [(i,j) | i <- M.odarts m,
                  let j = if elem i [x2,x3,y1,z1,y2,w1]
                          then if y2 == z1 then
                                 swap y1 x2 (swap x3 z1 i)
                               else
                                 swap y1 z1 (swap y2 x3 (swap x2 w1 i))
                          else act (M.alpha m) i] },
   g, r)

turn_ULT :: ULT -> ULT
turn_ULT = snd . toULTc . turn_root . toRTM'

-- let q = map (\t -> (t, flip_ULT t)) $ [t | t <- allcULT 5, isPlanar t]
-- *LamTri> let ts = map fst q
-- *LamTri> let us = map snd q
-- *LamTri> let xx = map (\u -> fromJust (findIndex (alpha u) ts)) us
-- *LamTri> map (map (+1)) $ permToCycles (zip [0..] xx)
-- [[32],[29],[26,31],[25,30],[23,24],[18,27],[16],[15,28],[14],[13,17],[12,22],[11,21],[10,20],[9,19],[4,8],[3,7],[2,6],[1,5]]

{-
> let t1 = L 0 (A (V 0) (A (V 1) (V 2)))
> let t2 = A (V 0) (L 1 (A (V 1) (V 2)))
> let m1 = toRTM' t1
> toULTc m1
([1,2],L 0 (A (V 0) (A (V 1) (V 2))))
> let m1' = root (unroot m1) 1 (-1)
> let t1' = toULTc m1'
> t1'
([8,2],A (V 2) (L 9 (A (V 9) (V 8))))
> alphac t1' ([2,0],t2)
True
-}

-- a rooted embedding of the petersen graph

petersenRTM :: RTM
petersenRTM =
  (M.OMap {
      M.odarts = [-3..30],
      M.sigma = cyclesToPerm ([-3] : [[3*i+1,3*i+2,3*i+3] | i <- [-1..9]]),
      M.alpha = cyclesToPerm [[-3,0],[1,-1],[-2,4],
                              [2,16],[3,14],[5,7],[6,19],[8,10],
                              [9,22],[11,13],[12,25],[15,28],[17,24],
                              [18,26],[20,27],[21,29],[23,30]]
      }, [], -3)

-- and its corresponding lambda term...
petersenULT = snd (toULTc petersenRTM)

{--
> printULT petersenULT
\u -> \x10 -> \x22 -> \x19 -> \x16 -> u(\x21 -> x22(x16(x10(x19(x21)))))
> inferPrintTerms [petersenULT]
 |- \u -> \x10 -> \x22 -> \x19 -> \x16 -> u(\x21 -> x22(x16(x10(x19(x21))))) : (((f -> b) -> a) -> ((e -> d) -> ((c -> b) -> ((f -> e) -> ((d -> c) -> a))))) 
--}

-- more combinators

tetraB :: ULT
tetraB = L 0 (L 1 (L 2 (A (V 0) (A (V 1) (V 2)))))

k4C :: ULT
k4C = L 0 (L 1 (L 2 (A (A (V 0) (V 2)) (V 1))))

{- 
> inferPrintTerms [tetraB,k4C]
 |- \x -> \y -> \z -> x(y(z)) : ((b -> a) -> ((c -> b) -> (c -> a))) 
 |- \x -> \y -> \z -> x(z)(y) : ((c -> (b -> a)) -> (b -> (c -> a)))
-}

-- eulerULT tetraB == 2
-- length (colorings tetraB) == 6

-- eulerULT k4C == 0
-- length (colorings k4C) == 6

-- eulerULT petersenULT == -2
-- length (colorings petersenULT) == 0

-- platonic solids

-- rooted cube
cubeRTM :: RTM
cubeRTM =
  (M.OMap {
      M.odarts = [0..27],
      M.sigma = cyclesToPerm [[0],[1,4,2],[3,12,13],[14,15,16],[17,18,19],[5,7,6],[8,26,9],[21,20,22],[23,27,24],[11,10,25]],
      M.alpha = cyclesToPerm [[0,1],[2,3],[13,14],[16,17],[19,7],[4,5],[12,11],[15,27],[18,20],[6,8],[9,10],[25,24],[23,22],[21,26]]
      }, [], 0)

-- rooted dodecahedron
dodecaRTM :: RTM
dodecaRTM =
  (M.OMap {
      M.odarts = [0..63],
      M.sigma = cyclesToPerm ([0] : [[3*i+1,3*i+2,3*i+3] | i <- [0..20]]),
      M.alpha = cyclesToPerm [[0,1],[2,16],[3,4],[5,19],[6,7],[8,27],[9,10],
                              [11,50],[12,13],[14,54],[15,17],[18,60],[20,63],
                              [21,22],[23,35],[24,25],[26,28],[29,31],[30,51],
                              [32,34],[33,45],[36,37],[38,62],[39,40],[41,56],
                              [42,43],[44,48],[46,52],[47,49],[53,55],[57,58],
                              [59,61]]
      }, [], 0)
  

-- rooted "truncated" (to make 3-regular) octahedron
fatoctaRTM :: RTM
fatoctaRTM =
  (M.OMap {
      M.odarts = [0..75],
      M.sigma = cyclesToPerm ([0] : [[3*i+1,3*i+2,3*i+3] | i <- [0..24]]),
      M.alpha = cyclesToPerm [[0,1],[2,21],[3,4],[5,22],[6,7],[8,27],[9,10],
                              [11,63],[12,13],[14,66],[15,16],[17,32],[18,19],
                              [20,28],[23,48],[24,25],[26,57],[29,31],[30,45],
                              [33,34],[35,69],[36,37],[38,72],[39,40],[41,50],
                              [42,43],[44,46],[47,49],[51,52],[53,75],[54,55],
                              [56,58],[59,74],[60,61],[62,64],[65,67],[68,70],
                              [71,73]]
      }, [], 0)
  

cubeULT = snd (toULTc cubeRTM)          -- \a -> \b -> \c -> \d -> a(\e -> b(c(d(e))))
dodecaULT = snd (toULTc dodecaRTM)      -- \a -> \b -> \c -> \d -> \e -> a(\f -> \g -> b(\h -> c(\i -> d(\j -> e(f(\k -> g(h(i(j(k))))))))))
fatoctaULT = snd (toULTc fatoctaRTM)    -- \a -> \b -> \c -> \d -> \e -> \f -> a(\g -> \h -> \i -> b(c(\j -> \k -> d(e(\l -> f(g)(\m -> h(i(j(k(l(m)))))))))))

-- tutte graph

rootedTutteGraph :: RTM
rootedTutteGraph =
  (M.OMap {
      M.odarts = [0..141],
      M.sigma = cyclesToPerm [[0],[1,2,3],[4,5,6],[7,8,9],[10,11,12],[13,14,15],
                              [16,17,18],[19,20,21],[22,23,24],[25,26,27],[28,29,30],
                              [31,32,33],[34,35,36],[37,38,39],[40,41,42],[43,44,45],
                              [46,47,48],[49,50,51],[52,53,63],[54,55,56],[57,58,59],
                              [60,61,62],[64,65,66],[67,68,69],[70,71,140],[72,73,74],
                              [75,76,77],[78,79,80],[81,82,83],[84,85,86],[87,88,89],
                              [90,91,92],[93,94,95],[96,97,98],[99,100,101],[102,103,141],
                              [104,105,106],[107,108,109],[110,111,112],[113,114,115],
                              [116,117,118],[119,120,121],[122,123,124],[125,126,127],
                              [128,129,130],[131,132,133],[134,135,136],[137,138,139]],
      M.alpha = cyclesToPerm [[0,1],[2,30],[3,4],[5,95],[6,7],[8,31],[9,10],
                              [11,39],[12,13],[14,66],[15,16],[17,140],[18,19],
                              [20,77],[21,22],[23,112],[24,25],[26,118],[27,28],
                              [29,124],[32,98],[33,34],[35,141],[36,37],[38,40],
                              [41,103],[42,43],[44,87],[45,46],[47,92],[48,49],
                              [50,104],[51,52],[53,54],[55,80],[56,57],[58,86],
                              [59,60],[61,64],[62,63],[65,67],[68,85],[69,70],
                              [71,72],[73,82],[74,75],[76,78],[79,81],[83,84],
                              [88,100],[89,90],[91,93],[94,96],[97,99],[101,102],
                              [105,130],[106,107],[108,133],[109,110],[111,113],
                              [114,136],[115,116],[117,119],[120,139],[121,122],
                              [123,125],[126,138],[127,128],[129,131],[132,134],
                              [135,137]]
                }, [], 0)
  

tutteULT = snd (toULTc rootedTutteGraph)

