-- simple routines for generating and analyzing combinatorial maps
module Maps where

import Control.Monad.State
import Control.Monad.List

import Data.List
import qualified Data.Tuple as DT

import Util
import Formulas
import Bijections

data OMap = OMap { odarts :: [Int], sigma :: Perm, alpha :: Perm }
  deriving Show
data UMap = UMap { udarts :: [Int], x :: Perm, y :: Perm, z :: Perm }
  deriving Show

mapOMap :: OMap -> (Int -> Int) -> OMap
mapOMap m f =
  OMap { odarts = map f (odarts m),
         sigma = map (\(x,y) -> (f x,f y)) (sigma m),
         alpha = map (\(x,y) -> (f x,f y)) (alpha m) }

phiOM :: OMap -> Perm
phiOM m = comp (alpha m) (inv (sigma m))

cyclesOM :: OMap -> ([[Int]], [[Int]], [[Int]])
cyclesOM m = (permToCycles (sigma m), permToCycles (alpha m), permToCycles (phiOM m))

verticesOM :: OMap -> [[Int]]
verticesOM m = let (v,e,f) = cyclesOM m in v
edgesOM :: OMap -> [[Int]]
edgesOM m = let (v,e,f) = cyclesOM m in e
facesOM :: OMap -> [[Int]]
facesOM m = let (v,e,f) = cyclesOM m in f

eulerOM :: OMap -> Int
eulerOM m =
  let (v,e,f) = (length $ permToCycles (sigma m), length $ permToCycles (alpha m), length $ permToCycles (phiOM m)) in 
  v-e+f

passportOM :: OMap -> ([Int],[Int],[Int])
passportOM m = passport (sigma m,alpha m,phiOM m)

-- compute the underlying abstract graph of an oriented map (ignore dangling edges)
graphOM :: OMap -> ([Int],[(Int,Int)])
graphOM m =
  let vs = map head $ verticesOM m in
  let vertexOf i = head $ intersect (orbit (act (sigma m)) i) vs in
  (vs, map (\[i,j] -> (vertexOf i,vertexOf j)) (filter ((==2) . length) $ edgesOM m))

-- map dual = exchange vertices and faces
dualOM :: OMap -> OMap
dualOM m = OMap { odarts = odarts m, sigma = comp (inv (sigma m)) (alpha m), alpha = alpha m }

-- hypermap dual = exchange vertices and edges
dualHM :: OMap -> OMap
dualHM m = OMap { odarts = odarts m, sigma = alpha m, alpha = sigma m }

-- unification for unoriented maps (naive version)
naiveUnifyUM :: UMap -> UMap -> [Perm]
naiveUnifyUM m1 m2 =
  filter (\p -> x m2 `eqperm` conjugate p (x m1) &&
                y m2 `eqperm` conjugate p (y m1) &&
                z m2 `eqperm` conjugate p (z m1))
  [p | p <- inject (udarts m1) (udarts m2)]

-- unification for unoriented maps (less naive version)
fastUnifyUM :: UMap -> UMap -> [Perm]
fastUnifyUM m1 m2 =
  -- first check that they have the same vertex and face passport
  let (sigma1,phi1) = (comp (x m1) (y m1), comp (y m1) (z m1)) in
  let (sigma2,phi2) = (comp (x m2) (y m2), comp (y m2) (z m2)) in
  if clengths sigma1 /= clengths sigma2 ||
     clengths phi1 /= clengths phi2 then [] else
  -- then try to find a unifier
  filter (\p -> x m2 `eqperm` conjugate p (x m1) && y m2 `eqperm` conjugate p (y m1) && z m2 `eqperm` conjugate p (z m1))
  [p | p <- residual sigma1 sigma2]

-- unification for oriented maps
naiveUnifyOM :: OMap -> OMap -> [Perm]
naiveUnifyOM m1 m2 =
  filter (\p -> sigma m2 `eqperm` conjugate p (sigma m1) &&
                alpha m2 `eqperm` conjugate p (alpha m1))
  [p | p <- inject (odarts m1) (odarts m2)]

fastUnifyOM :: OMap -> OMap -> [Perm]
fastUnifyOM m1 m2 =
  let phi1 = phiOM m1 in
  let phi2 = phiOM m2 in
  -- first check that they have the same vertex and face passports
  if clengths (sigma m1) /= clengths (sigma m2) ||
     clengths phi1 /= clengths phi2 then [] else
  -- then try to find a unifier
  filter (\p -> sigma m2 `eqperm` conjugate p (sigma m1) &&
                alpha m2 `eqperm` conjugate p (alpha m1))
  [p | p <- residual phi1 phi2]

unifyOMat :: OMap -> OMap -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> Maybe [(Int,Int)]

unifyOMat m1 m2 [] [] matching = Just matching
unifyOMat m1 m2 ((d1,d2):remainv) remaine matching =
--  case (lookup d1 matching, lookup d2 (map DT.swap matching)) of
  case (lookup d1 matching) of
--    (Just d2',Just d1') -> if d1 == d1' && d2 == d2' then unifyOMat m1 m2 remainv remaine matching else Nothing
    (Just d2') -> if d2 == d2' then unifyOMat m1 m2 remainv remaine matching else Nothing
    -- (Just _,Nothing) -> Nothing
    -- (Nothing,Just _) -> Nothing
    (Nothing) ->
      let s1d1 = tail $ orbit (act (sigma m1)) d1 in
      let s2d2 = tail $ orbit (act (sigma m2)) d2 in
      if length s1d1 == length s2d2 then
        unifyOMat m1 m2 remainv (zip s1d1 s2d2 ++ remaine) ((d1,d2):matching)
      else Nothing
unifyOMat m1 m2 [] ((d1,d2):remaine) matching =
  let a1d1 = act (alpha m1) d1 in
  let a2d2 = act (alpha m2) d2 in
  unifyOMat m1 m2 [(a1d1,a2d2)] remaine ((d1,d2):matching)


-- two maps are equivalent if they have a unifier
equivUM :: UMap -> UMap -> Bool
equivUM m1 m2 = any (\_ -> True) (fastUnifyUM m1 m2)

equivOM :: OMap -> OMap -> Bool
equivOM m1 m2 =
  any (\_ -> True) (fastUnifyOM m1 m2)
  -- let d1 = head (odarts m1) in
  -- any (\d2 -> unifyOMat m1 m2 [(d1,d2)] [(d1,d2)] [] /= Nothing) (odarts m2)

-- two rooted maps are equivalent if they have a unifier which respects the rooting
equivRUM :: UMap -> UMap -> Bool
equivRUM m1 m2 =
  let root1 = head (udarts m1) in
  let root2 = head (udarts m2) in
  any (\p -> act p root1 == root2) (fastUnifyUM m1 m2)

equivROM :: OMap -> OMap -> Bool
equivROM m1 m2 =
  let root1 = head (odarts m1) in
  let root2 = head (odarts m2) in
--  any (\p -> act p root1 == root2) (fastUnifyOM m1 m2)
  unifyOMat m1 m2 [(root1,root2)] [(root1,root2)] [] /= Nothing

-- unoriented maps
genUMap :: Int -> [UMap]
genUMap n =
  [UMap {udarts=[1..4*n], x=x, y=y, z=z} |
   let x = [(4*i+1,4*i+2) | i <- [0..n-1]] ++ [(4*i+2,4*i+1) | i <- [0..n-1]] ++
           [(4*i+3,4*i+4) | i <- [0..n-1]] ++ [(4*i+4,4*i+3) | i <- [0..n-1]],
   let z = [(4*i+1,4*i+4) | i <- [0..n-1]] ++ [(4*i+4,4*i+1) | i <- [0..n-1]] ++
           [(4*i+3,4*i+2) | i <- [0..n-1]] ++ [(4*i+2,4*i+3) | i <- [0..n-1]],
   y <- involute [1..4*n],
   isTransitive [x,y,z] [1] [1..4*n]]

-- [length $ nubBy equivRUM $ genUMap n | n <- [1..]] == [3,24,297,...]

-- oriented maps
genOMap :: Int -> [OMap]
genOMap 0 = [OMap {odarts=[],sigma=[],alpha=[]}]
genOMap n =
  [OMap {odarts=[1..2*n], sigma=sigma, alpha=alpha} |
   let alpha = [(2*i+1,2*i+2) | i <- [0..n-1]] ++ [(2*i+2,2*i+1) | i <- [0..n-1]],
   sigma <- permute [1..2*n],
   isTransitive [sigma,alpha] [1] [1..2*n]]

-- [length $ nubBy equivOM $ genOMap n | n <- [1..]] == [2,5,20,107,...] == A006867?? or A214816??
-- [length $ nubBy equivROM $ genOMap n | n <- [1..]] == [2,10,74,706,...] == A000698 

-- [sum $ map (\m -> length $ permToCycles (sigma m)) $ nubBy equivROM $ genOMap n | n <- [1..]] == [3,19,165,1769,...]

-- generate "intrinsically-rooted" maps, where the root is marked by a single
-- fixed point of the edge involution.
genROM :: Int -> [OMap]
genROM 0 = [OMap {odarts=[0],sigma=[(0,0)],alpha=[(0,0)]}]
genROM n =
  [OMap {odarts=odarts',sigma=sigma',alpha=alpha'} |
   m <- genOMap n,
   let r = 0,
   let odarts' = r:odarts m,
   let alpha' = (r,r):alpha m,
   (c,cs) <- remove $ permToCycles (sigma m),
   let sigma' = cyclesToPerm ((r:c):cs)]

-- [length $ nubBy equivOM $ genROM n | n <- [0..]] == [1,2,10,74,...]

genRPM n = [m | m <- genROM n, eulerOM m == 1]
-- [length $ nubBy equivOM $ genRPM n | n <- [0..]] == [1,2,9,54,...]


-- generate rooted maps inductively via Tutte decomposition
genROM_tutte :: Int -> StateT Int [] (Int,OMap)
genROM_tutte 1 = do
  r <- freshInt
  return (r,OMap {odarts=[r],sigma=[(r,r)],alpha=[(r,r)]})
genROM_tutte n = genROM_isthmic n  `mplus` genROM_nonisthmic n
  where
    genROM_isthmic :: Int -> StateT Int [] (Int,OMap)
    genROM_isthmic n = do
      i <- lift [1..n-1]
      (r1,m1) <- genROM_tutte i
      (r2,m2) <- genROM_tutte (n-i)
      r <- freshInt
      let odarts' = r : odarts m1 ++ odarts m2
      let sigma' = [(r,r1)] ++
                   [(d,if d' == r1 then r else d') | d <- odarts m1, let d' = act (sigma m1) d] ++
                   sigma m2
      let alpha' = [(r,r)] ++
                   [(r1,r2),(r2,r1)] ++
                   [(d,d') | d <- odarts m1 \\ [r1], let d' = act (alpha m1) d] ++
                   [(d,d') | d <- odarts m2 \\ [r2], let d' = act (alpha m2) d]
      return (r, OMap { odarts = odarts', sigma = sigma', alpha = alpha' })

    genROM_nonisthmic :: Int -> StateT Int [] (Int,OMap)
    genROM_nonisthmic n = do
      (r1,m1) <- genROM_tutte (n-1)
      r2 <- freshInt
      r <- freshInt
      x <- lift (odarts m1)
      let odarts' = r : r2 : odarts m1
      let r2' = if act (sigma m1) x == r1 then r else act (sigma m1) x
      let sigma' = [(r,r1)] ++ [(x,r2),(r2,r2')] ++
                   [(d,if d' == r1 then r else d') | d <- odarts m1 \\ [x], let d' = act (sigma m1) d]
      let alpha' = [(r,r)] ++
                   [(r1,r2),(r2,r1)] ++
                   [(d,d') | d <- odarts m1 \\ [r1], let d' = act (alpha m1) d]
      return (r, OMap { odarts = odarts', sigma = sigma', alpha = alpha' })

genROM_tutte' :: Int -> [(Int,OMap)]
genROM_tutte' n = map fst $ runStateT (genROM_tutte n) 0

-- list of darts incident to root vertex
rootDartsROM :: OMap -> [Int]
rootDartsROM m =
  let r = head (odarts m) in
  orbit (act (sigma m)) r

-- list of edges (rather than darts) incident to root vertex
rootEdgesROM :: OMap -> [[Int]]
rootEdgesROM m =
  let r = head (odarts m) in
  let ds = orbit (act (sigma m)) r \\ [r] in
  equivClassesBy (\d1 d2 -> elem d1 (orbit (act (alpha m)) d2)) ds []

-- list of faces incident to root vertex
rootFacesROM :: OMap -> [[Int]]
rootFacesROM m =
  let r = head (odarts m) in
  let ds = orbit (act (sigma m)) r \\ [r] in
  equivClassesBy (\d1 d2 -> elem d1 (orbit (act (comp (inv (sigma m)) (alpha m))) d2)) ds []

-- list of vertices incident to root face
rootVerticesROM :: OMap -> [[Int]]
rootVerticesROM m =
  let r = head (odarts m) in
  let ds = orbit (act (phiOM m)) r \\ [r] in
  equivClassesBy (\d1 d2 -> elem d1 (orbit (act (sigma m)) d2)) ds []


-- map (length . rootDartsROM) (nubBy equivOM $ genROM 1) == [2,3]
-- map (length . rootEdgesROM) (nubBy equivOM $ genROM 1) == [1,1]
-- map (length . rootDartsROM) (nubBy equivOM $ genROM 2) == [2,3,4,2,4,5,4,5,3,5]
-- map (length . rootEdgesROM) (nubBy equivOM $ genROM 2) == [1,2,2,1,2,2,2,2,2,2]
-- map (length . rootDartsROM) (nubBy equivOM $ genROM 3) == [2,3,3,4,3,2,2,5,2,6,2,2,5,6,2,2,4,5,3,4,2,6,2,3,4,4,3,4,4,6,7,6,7,5,6,3,5,7,3,4,6,7,6,7,5,6,5,7,4,5,6,7,6,7,4,7,4,5,3,5,3,6,7,5,6,5,7,6,7,7,7,7,4,6]
-- map (length . rootEdgesROM) (nubBy equivOM $ genROM 3) == [1,2,2,2,2,1,1,3,1,3,1,1,3,3,1,1,3,3,2,3,1,3,1,2,2,2,2,2,2,3,3,3,3,3,3,2,3,3,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,3,2,3,3,3,3,3,3,3,3,3,3,3,3,3]

-- map (length . rootDartsROM) (nubBy equivOM $ genRPM 2) == [2,3,4,2,4,5,4,5,3]
-- map (length . rootEdgesROM) (nubBy equivOM $ genRPM 2) == [1,2,2,1,2,2,2,2,2]
-- map (length . rootDartsROM) (nubBy equivOM $ genRPM 3) == [2,3,3,4,3,2,2,5,2,6,2,2,5,6,2,2,4,5,3,4,2,3,4,4,3,4,4,6,7,6,7,5,6,3,5,3,4,6,7,6,7,5,6,5,4,5,4,5,3,5,6,6,7,4]
-- map (length . rootEdgesROM) (nubBy equivOM $ genRPM 3) == [1,2,2,2,2,1,1,3,1,3,1,1,3,3,1,1,3,3,2,3,1,2,2,2,2,2,2,3,3,3,3,3,3,2,3,2,2,3,3,3,3,3,3,3,3,3,3,3,2,3,3,3,3,3]

unroot :: OMap -> OMap
unroot m =
  let edges = permToCycles (alpha m) in
  let (fulledges,hyperedges) = partition (\c -> length c == 2) edges in
  case hyperedges of
    [[r]] ->
      let r' = act (sigma m) r in
      OMap { odarts = odarts m \\ [r],
             sigma = [(i,if j == r then r' else j) | (i,j) <- sigma m, i /= r],
             alpha = cyclesToPerm fulledges }
    _ -> error "unroot: alpha must have exactly one fixpoint and n 2-cycles"

-- [length $ nubBy equivOM $ map unroot $ genROM n | n <- [1..]] == [2,5,20,...]

genOM3 :: Int -> [OMap]
genOM3 n =
  [OMap {odarts=[1..6*n], sigma=sigma, alpha=alpha} |
   let alpha = [(2*i+1,2*i+2) | i <- [0..3*n-1]] ++ [(2*i+2,2*i+1) | i <- [0..3*n-1]],
   sigma <- cubic [1..6*n],
   isTransitive [sigma,alpha] [1] [1..6*n]]
  where
    cubic :: [a] -> [[(a,a)]]
    cubic [] = return []
    cubic (a1:as) = do
      (a2,as') <- remove as
      (a3,as'') <- remove as'
      p <- cubic as''
      return ((a1,a2):(a2,a3):(a3,a1):p)

genChordDiagram :: Int -> [OMap]
genChordDiagram n =
  [m |
   m <- genOM3 n,
   let r = head (odarts m),
   let rface = orbit (act (phiOM m)) r,
   length rface == 2*n,
   let vs = verticesOM m,
   all (\v -> intersect v rface /= []) vs]

genOM4 :: Int -> [OMap]
genOM4 n =
  [OMap {odarts=[1..4*n], sigma=sigma, alpha=alpha} |
   let alpha = [(2*i+1,2*i+2) | i <- [0..2*n-1]] ++ [(2*i+2,2*i+1) | i <- [0..2*n-1]],
   sigma <- quartic [1..4*n],
   isTransitive [sigma,alpha] [1] [1..4*n]]
  where
    quartic :: [a] -> [[(a,a)]]
    quartic [] = return []
    quartic (a1:as) = do
      (a2,as') <- remove as
      (a3,as'') <- remove as'
      (a4,as''') <- remove as''
      p <- quartic as'''
      return ((a1,a2):(a2,a3):(a3,a4):(a4,a1):p)

-- [length $ nubBy equivROM $ genOM4 n | n <- [1..]] == [3,24,...] == ?

-- hypermaps with n darts
genHMap :: Int -> [OMap]
genHMap n =
  [OMap {odarts=[1..n], sigma=sigma, alpha=alpha} |
   -- we can always use a labelling of the darts in which the cycles of the first
   -- permutation are increasing.  So, for sigma we just generate a partition of [1..n].
   sigma0 <- parts [1..n],
   let sigma = cyclesToPerm sigma0,
   alpha <- permute [1..n],
   isTransitive [sigma,alpha] [1] [1..n]]

-- [length $ nubBy equivOM $ genHMap n | n <- [1..]] == [1,3,7,26,97,...] == A057005
-- [length $ nubBy equivROM $ genHMap n | n <- [1..]] == [1,3,13,71,461,...] == A003319

-- encoding face 2-colored 4-valent maps (in bijection with rooted
-- oriented maps) à la Zinn/Justin-Zuber [arXiv:math-ph/0303049], as
-- hypermaps (sigma,alpha) on 2n darts, satisfying the extra condition
-- that the permutation sigma^{-1}alpha is a fixed-point free
-- involution.
genZJZ :: Int -> [OMap]
genZJZ n = [m | m <- genHMap (2*n), fpfreeInvolution (comp (inv (sigma m)) (alpha m))]

-- [length $ nubBy equivROM $ genZJZ n | n <- [1..]] == [2,10,74,706,...] == A000698
-- [length $ nubBy equivOM $ genZJZ n | n <- [1..]] == [2,5,20,107,...]

-- hypermaps with n darts, in which the first permutation has k cycles
genHMapByCycles :: Int -> Int -> [OMap]
genHMapByCycles n k =
  [OMap {odarts=[1..n], sigma=sigma, alpha=alpha} |
   sigma0 <- stirling0 k [1..n],
   let sigma = cyclesToPerm sigma0,
   alpha <- permute [1..n],
   isTransitive [sigma,alpha] [1] [1..n]]

-- [length $ nubBy equivOM $ genHMapByCycles n 1 | n <- [1..]] == [1,2,4,10,28,136,...] == A061417
-- [length $ nubBy equivROM $ genHMapByCycles n 1 | n <- [1..]] == [1,2,6,24,120,720,...] == n!

genZJZByCycles :: Int -> Int -> [OMap]
genZJZByCycles n k = [m | m <- genHMapByCycles (2*n) k, fpfreeInvolution (comp (inv (sigma m)) (alpha m))]

-- [length $ nubBy equivROM $ genZJZByCycles n 1 | n <- [1..]] == [1,3,15,105,...] == A001147
-- [length $ nubBy equivROM $ genZJZByCycles n 2 | n <- [1..]] == [1,5,32,260,...]
-- [length $ nubBy equivROM $ genZJZByCycles n 3 | n <- [1..]] == [0,2,22,234,...]

-- [sum [(length $ permToCycles (sigma m)) | m <- nubBy equivROM $ genZJZ n] | n <- [1..]] == [3,19,165,...]
-- [sum [binom 2 (length $ permToCycles (sigma m)) | m <- nubBy equivROM $ genZJZ n] | n <- [1..]] == [1,11,128,...]
-- [sum [binom 3 (length $ permToCycles (sigma m)) | m <- nubBy equivROM $ genZJZ n] | n <- [1..]] == [0,2,42,746,...]

-- a "slightly hypermap" is a hypermap in which all but one white vertex has degree 2.  The indices k and n count the number of vertices and the number of ordinary edges, respectively.
genSHMap :: Int -> Int -> [OMap]
genSHMap k n =
  [m |
   m <- genHMapByCycles (2*n + k) k,
   let cs = clengths (alpha m),
   sum [1 | c <- cs, c == 2] == n &&
   sum [1 | c <- cs, c == k] == 1]

-- EXAMPLES

-- the Petersen graph as an oriented map
petersenOM :: OMap
petersenOM =
  OMap {
    odarts = [1..30],
    sigma = cyclesToPerm [[3*i+1,3*i+2,3*i+3] | i <- [0..9]],
    alpha = cyclesToPerm [[1,4],[2,16],[3,14],[5,7],[6,19],[8,10],
                          [9,22],[11,13],[12,25],[15,28],[17,24],
                          [18,26],[20,27],[21,29],[23,30]]
    }


-- test if the edge corresponding to a dart is a bridge
isBridge :: OMap -> Int -> Bool
isBridge m d =
  let d' = act (alpha m) d in
  let alpha' = [(x,y) | x <- odarts m, let y = if x == d then d else if x == d' then d' else act (alpha m) x] in
  not $ elem d' (transClosure [sigma m, alpha'] [d])

-- test if a map is bridgeless
isBridgeless :: OMap -> Bool
isBridgeless m = not $ any (isBridge m) (odarts m)

-- [length $ filter (isBridgeless . unroot) (map snd $ genROM_tutte' n) | n <- [1..]] == [1,1,4,27,248,2830,38232,...] == A000699

-- check that a map is connected
isConnected :: OMap -> Bool
isConnected m =
  if odarts m == [] then True else -- we count the empty map as connected
  let r = head (odarts m) in
  length (transClosure [sigma m, alpha m] [r]) == length (odarts m)

-- delete a list of darts from a map
deleteDarts :: OMap -> [Int] -> OMap
deleteDarts m ds =
  let odarts' = odarts m \\ ds in
  let sigma' d = let d' = act (sigma m) d in if elem d' ds then sigma' d' else d' in
  let alpha' d = let d' = act (alpha m) d in if elem d' ds then alpha' d' else d' in
  OMap {
    odarts = odarts',
    sigma = [(d,sigma' d) | d <- odarts'],
    alpha = [(d,alpha' d) | d <- odarts']
    }

-- detach (but don't delete) the darts incident to a vertex
breakVertex :: OMap -> [Int] -> OMap
breakVertex m v =
  OMap {
    odarts = odarts m,
    sigma = [(d,d') | d <- odarts m, let d' = if elem d v then d else act (sigma m) d],
    alpha = alpha m
    }
  
-- detach (but don't delete) the darts incident to an edge
breakEdge :: OMap -> [Int] -> OMap
breakEdge m e =
  OMap {
    odarts = odarts m,
    sigma = sigma m,
    alpha = [(d,d') | d <- odarts m, let d' = if elem d e then d else act (alpha m) d]
    }

-- test if a map is k-vertex-connected
isKVConnected :: Int -> OMap -> Bool
isKVConnected 1 m = isConnected m
isKVConnected k m = 
  let vs = verticesOM m in
  length vs > k &&
  all (\x -> isKVConnected (k-1) (breakVertex m x)) vs

-- test if a map is k-edge-connected
isKEConnected :: Int -> OMap -> Bool
isKEConnected 1 m = isConnected m
isKEConnected k m = 
  let es = edgesOM m in
  length es > 2 &&
  all (\x -> isKEConnected (k-1) (breakEdge m x)) es

