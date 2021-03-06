-- general routines for linear lambda calculus (alpha-equivalence, normalization, type-inference, etc.)

module Lambda where

import Control.Monad
import Control.Monad.State
import Control.Monad.List
import Data.IORef
import Data.Maybe
import Data.List

import Util
import Formulas
import Bijections

import qualified Catalan as C

-- type of raw linear lambda terms
data ULTp v = V v | A (ULTp v) (ULTp v) | L v (ULTp v)
  deriving (Eq,Show)
type ULT = ULTp Int
-- type of linear lambda terms with explicit context
type ULTc = ([Int],ULT)

rename :: (Int,Int) -> ULT -> ULT
rename (x1,x2) (V x) = if x == x1 then V x2 else V x
rename (x1,x2) (A t u) = A (rename (x1,x2) t) (rename (x1,x2) u)
rename (x1,x2) (L x u) = if x == x1 then L x u else L x (rename (x1,x2) u)

-- alpha-equivalence of raw terms
-- we assume that all lambdas bind distinct variables, named by non-negative integers
alpha :: ULT -> ULT -> Bool
alpha (V x1) (V x2) = x1 == x2
alpha (A t1 u1) (A t2 u2) = alpha t1 t2 && alpha u1 u2
alpha (L x1 t1) (L x2 t2) = alpha
                            (rename (x1,(-x1-1)) t1)
                            (rename (x2,(-x1-1)) t2)
alpha _ _ = False
-- alpha-equivalence of terms in context
alphac :: ULTc -> ULTc -> Bool
alphac ([],t1) ([],t2) = alpha t1 t2
alphac (x1:g1,t1) (x2:g2,t2) = alphac (g1,rename (x1,(-x1-1)) t1) (g2,rename (x2,(-x1-1)) t2)
alphac _ _ = False

-- free variables of a raw term
freevars :: ULT -> [Int]
freevars (V x) = [x]
freevars (A t u) = freevars t ++ freevars u
freevars (L x t) = freevars t \\ [x]

-- list of subterms
subterms :: ULT -> [ULT]
subterms (t@(V x)) = [V x]
subterms (t@(A t1 t2)) = t : subterms t1 ++ subterms t2
subterms (t@(L x t1)) = t : subterms t1

-- lambda lifting of a term in context
closeULTc :: ULTc -> ULT
closeULTc ([], t) = t
closeULTc (x:g, t) = L x (closeULTc (g,t))

-- test if term is a lambda, application, or variable
isLam :: ULT -> Bool
isLam (L _ _) = True
isLam _ = False
isApp :: ULT -> Bool
isApp (A _ _) = True
isApp _ = False
isVar :: ULT -> Bool
isVar (V _) = True
isVar _ = False

-- remove an outermost lambda
unlambda :: ULT -> ULT
unlambda (L x t) = t

-- coerce a variable to an integer
unVar :: ULT -> Int
unVar (V x) = x

-- remove string of outermost lambdas
unlambdas :: ULT -> ([Int],ULT)
unlambdas (L x t) = let (vars,r) = unlambdas t in (x:vars,r)
unlambdas t = ([],t)

-- get argument of an application
argapp :: ULT -> ULT
argapp (A t u) = u

-- get function of an application
argfn :: ULT -> ULT
argfn (A t u) = t

-- collect arguments and head variable of an iterated application
unapps :: ULT -> [ULT] -> (Int,[ULT])
unapps (A t u) args = unapps t (u:args)
unapps (V x) args = (x,args)

-- return the head variable of an iterated application
headvar :: ULT -> Int
headvar t = fst (unapps t [])

-- build an iterated application given a head variable and list of arguments
apps :: Int -> [ULT] -> ULT
apps x ts = foldl A (V x) ts

-- collect arguments of a variable occurring in a term
varargs_cps :: ULT -> Int -> [ULT] -> [ULT] -> [ULT]
varargs_cps (L x t) y succ fail = varargs_cps t y succ fail
varargs_cps (A t u) y succ fail = varargs_cps t y (u:succ) $
                                  varargs_cps u y [] fail
varargs_cps (V x) y succ fail = if x == y then succ else fail

varargs :: ULT -> Int -> [ULT]
varargs t y = varargs_cps t y [] []

{-- some pretty printing routines... --}

paren :: String -> String
paren s = "(" ++ s ++ ")"

prettyvar :: Int -> String
prettyvar i = (["x","y","z","w","u","v"] ++ ["x" ++ show i | i <- [0..]]) !! i

prettyULT :: ULT -> String
prettyULT (V x) = prettyvar x
prettyULT (A (L x t) u) = paren (prettyULT (L x t)) ++ paren (prettyULT u)
prettyULT (A t u) = (if isLam t then paren else id) (prettyULT t) ++ paren (prettyULT u)
prettyULT (L x t) = "\\" ++ prettyvar x ++ " -> " ++ prettyULT t

prettyULTc :: ULTc -> String
prettyULTc (g,t) = "[" ++ intercalate "," (map prettyvar g) ++ "]" ++ prettyULT t

printULT :: ULT -> IO ()
printULT t = putStr (prettyULT t ++ "\n")
printULTc :: ULTc -> IO ()
printULTc gt = putStr (prettyULTc gt ++ "\n")

-- Here we define various procedures for generating different families
-- of linear lambda terms.

-- "genULTp sp ins env n"
genULTp :: ([Int] -> [([Int],[Int])]) -> (Int -> [Int] -> [[Int]]) -> [Int] -> Int -> StateT Int [] ULT
-- Generates linear lambda terms in a given context of free variables env,
-- with n lambdas/apps. The parameter "sp" is a function which returns all
-- allowable splittings of the context, while the parameter "ins" is a
-- function which returns all ways of extending a context by a variable.
-- These can be varied, for example, to generate planar terms or
-- indecomposable terms.
-- This procedure runs in the State monad in order to generate fresh
-- variable names.
genULTp sp ins [x] 0 = return (V x)
genULTp sp ins _ 0 = mzero
genULTp sp ins env n = genA env n `mplus` genL env n
 where
   genA :: [Int] -> Int -> StateT Int [] ULT
   genA env n = do
     (env1,env2) <- lift (sp env)
     i <- lift [0..n-1]
     t <- genULTp sp ins env1 i
     u <- genULTp sp ins env2 (n-i-1)
     return (A t u)
   genL :: [Int] -> Int -> StateT Int [] ULT
   genL env n = do
     x <- freshInt
     env' <- lift (ins x env)
     t <- genULTp sp ins env' (n-1)
     return (L x t)

-- all closed linear lambda terms
allcULT :: Int -> [ULT]
allcULT n = map fst $ runStateT (genULTp split (\x g -> [x:g]) [] n) 0

-- all linear lambda terms with one free variable
allvULT :: Int -> Int -> [ULT]
allvULT x n = map fst $ runStateT (genULTp split (\x g -> [x:g]) [x] n) (x+1)

-- all closed indecomposable terms
allcULTnb :: Int -> [ULT]
allcULTnb n = map fst $ runStateT (genULTp sp (\x g -> [x:g]) [] n) 0
  where
    sp = filter (\(g1,g2) -> g1 /= [] && g2 /= []) . split

-- all closed planar lambda terms (turn on bit for LR-planarity)
allcUPT :: Bool -> Int -> [ULT]
allcUPT lr n = map fst $ runStateT (genULTp splitC ins [] n) 0
  where
    ins = if lr then (\x g -> [x:g]) else (\x g -> [g ++ [x]])
                                     
-- all closed planar indecomposable lambda terms (turn on bit for LR-planarity)
allcUPTnb :: Bool -> Int -> [ULT]
allcUPTnb lr n = map fst $ runStateT (genULTp splitCN ins [] n) 0
  where
    ins = if lr then (\x g -> [x:g]) else (\x g -> [g ++ [x]])

-- "genNeutralp sp mg env n"
-- "genNormalp sp mg env n"
genNeutralp :: ([Int] -> [([Int],[Int])]) -> ([Int] -> [Int] -> [[Int]]) -> [Int] -> Int -> StateT Int [] ULT
genNormalp  :: ([Int] -> [([Int],[Int])]) -> ([Int] -> [Int] -> [[Int]]) -> [Int] -> Int -> StateT Int [] ULT
-- These procedures directly generate neutral and normal terms in a given
-- context env, with n neutral-normal phase changes. The parameter "sp" is
-- a function which returns all allowable splittings of the context, while
-- the parameter "mg" is a function which returns all ways of merging two
-- contexts.
-- These procedures run in the State monad in order to generate fresh
-- variable names.
genNeutralp sp mg [x] 0 = return (V x)
genNeutralp sp mg _ 0 = mzero
genNeutralp sp mg env n = do
  (env1,env2) <- lift (sp env)
  i <- lift [0..n-1]
  t <- genNeutralp sp mg env1 i
  u <- genNormalp sp mg env2 (n-i)
  return (A t u)
genNormalp sp mg env n = do
  k <- lift [0..n-length env]
  xs <- freshInts k
  env' <- lift (mg xs env)
  t <- genNeutralp sp mg env' (n-1)
  return $ foldr L t xs

-- all normal linear terms
allcNLT :: Int -> [ULT]
allcNLT n = [t | t <- map fst $ runStateT (genNormalp sp mg [] n) 0]
  where
    sp = split
    mg g1 g2 = [g1 ++ g2]

-- all neutral linear terms
allNeutral :: Int -> [ULTc]
allNeutral n = [([0..k-1],t) | k <- [0..n+1], t <- map fst $ runStateT (genNeutralp sp mg [0..k-1] n) k]
  where
    sp = split
    mg g1 g2 = [g1 ++ g2]

-- all normal indecomposable terms
allcNLTnb :: Int -> [ULT]
allcNLTnb n = [t | t <- map fst $ runStateT (genNormalp sp mg [] n) 0]
  where
    sp = filter (\(g1,g2) -> g2 /= []) . split
    mg g1 g2 = [g1 ++ g2]

-- all normal planar terms (turn on bit for LR-planarity)
allcNPT :: Bool -> Int -> [ULT]
allcNPT lr n = [t | t <- map fst $ runStateT (genNormalp sp mg [] n) 0]
  where
    sp = splitC
    mg xs g = [if lr then reverse xs ++ g else g ++ xs]
    
-- normal indecomposable planar terms (turn on bit for LR)
allcNPTnb :: Bool -> Int -> [ULT]
allcNPTnb lr n = [t | t <- map fst $ runStateT (genNormalp sp mg [] n) 0]
  where
    sp = splitCN
    mg xs g = [if lr then reverse xs ++ g else g ++ xs]

-- representatives for all equivalence classes of normal linear terms modulo
-- free exchange of lambdas.
allcNLTex n = [t | t <- map fst $ runStateT (genNormalp sp mg [] n) 0]
  where
    sp = splitC
    mg xs env = shuffle xs env

-- representatives for all equivalence classes of normal linear indecomposable
-- terms modulo free exchange of lambdas.
allcNLTexnb n = [t | t <- map fst $ runStateT (genNormalp sp mg [] n) 0]
  where
    sp = splitCN
    mg = shuffle

-- more decomposition routines

unNeutral :: ULT -> [ULT] -> (Int,[ULT])
unNeutral (V x) args = (x,args)
unNeutral (A t u) args = unNeutral t (u:args)
unNeutral (L _ _) args = error "neutralArgs: term not neutral"

unNormal :: ULT -> [Int] -> ([Int],ULT)
unNormal (L x t) vars = unNormal t (x:vars)
unNormal t vars = (vars,t)

-- test a lambda term for normality (= absence of beta-redexes)
-- isNormal defined in mutual induction with isNeutral
isNormal :: ULT -> Bool
isNeutral :: ULT -> Bool
isNormal (L _ t) = isNormal t
isNormal t = isNeutral t
isNeutral (V _) = True
isNeutral (A t u) = isNeutral t && isNormal u
isNeutral (L _ _) = False

-- test for (in)decomposability
isDecomposable :: ULT -> Bool
isDecomposable (V _) = False
isDecomposable (A t1 t2) = freevars t1 == [] || freevars t2 == [] || isDecomposable t1 || isDecomposable t2
isDecomposable (L _ t) = isDecomposable t

isIndecomposable t = not (isDecomposable t)

-- neutral subterms of a normal/neutral term

neutral_subnormals :: ULT -> [ULT]
neutral_subneutrals :: ULT -> [ULT]
neutral_subnormals (L x t1) = neutral_subnormals t1
neutral_subnormals t = neutral_subneutrals t

neutral_subneutrals (t@(A t1 t2)) = t : neutral_subneutrals t1 ++ neutral_subnormals t2
neutral_subneutrals (t@(V x)) = [t]

-- normal subterms of a normal/neutral term

normal_subnormals :: ULT -> [ULT]
normal_subneutrals :: ULT -> [ULT]
normal_subnormals (t@(L x t1)) = t : normal_subnormals t1
normal_subnormals t = t : normal_subneutrals t

normal_subneutrals (A t1 t2) = normal_subneutrals t1 ++ normal_subnormals t2
normal_subneutrals (V x) = []

-- subnormal subterms (in Jason's terminology) of a normal/neutral term

subnormal_subsubnormals :: ULT -> [ULT]
subnormal_subnormals :: ULT -> [ULT]
subnormal_subneutrals :: ULT -> [ULT]
subnormal_subsubnormals t = t : subnormal_subnormals t
subnormal_subnormals (L x t1) = subnormal_subnormals t1
subnormal_subnormals t = subnormal_subneutrals t
subnormal_subneutrals (A t1 t2) = subnormal_subneutrals t1 ++ subnormal_subsubnormals t2
subnormal_subneutrals (V x) = []

-- size of a normal/neutral term, measured by number of neutral-normal switches

sizeNormal :: ULT -> Int
sizeNeutral :: ULT -> Int
sizeNormal (L x t) = sizeNormal t
sizeNormal t = 1+sizeNeutral t
sizeNeutral (A t u) = sizeNeutral t + sizeNormal u
sizeNeutral (V x) = 0
  
{-- NORMALIZATION OF LINEAR LAMBDA TERMS --}

-- type of one-hole contexts for linear lambda terms
data ULT' = V' | A'1 ULT' ULT | A'2 ULT ULT' | L' Int ULT'
  deriving (Eq,Show)

-- plug a one-hold context with a term to produce a term
plug :: ULT' -> ULT -> ULT
plug V' t0 = t0
plug (A'1 c u) t0 = A (plug c t0) u
plug (A'2 t c) t0 = A t (plug c t0)
plug (L' x c) t0 = L x (plug c t0)

-- return the list of beta-redexes in a term t.  A redex is represented as
-- a triple (t',u,c), where A t' u is a beta-redex and t == plug c (A t' u).
getRedex :: ULT -> [(ULT,ULT,ULT')]
getRedex (V _) = []
getRedex (A t u) = [(t,u,V') | isLam t] ++
                   [(t1,u1,A'1 r1 u) | (t1,u1,r1) <- getRedex t] ++
                   [(t2,u2,A'2 t r2) | (t2,u2,r2) <- getRedex u]
getRedex (L x t) = [(t1,u1,L' x r1) | (t1,u1,r1) <- getRedex t]

-- substitute a term for a variable in a term
subst :: (ULT,Int) -> ULT -> ULT
subst (t0,x) (V y) = if x == y then t0 else (V y)
subst (t0,x) (A t1 t2) = A (subst (t0,x) t1) (subst (t0,x) t2)
subst (t0,x) (L y t1) =
  if x == y then L y t1
  else if not $ elem y (freevars t0) then L y (subst (t0,x) t1)
  else
     let y' = 1 + foldr max x (freevars t0 ++ freevars t1) in
     L y' (subst (t0,x) $ subst (V y',y) t1)

-- step t computes all possible developments of t by one beta-redex
step :: ULT -> [ULT]
step t = do
  (L x t1,u,c) <- getRedex t
  return $ plug c (subst (u,x) t1)

-- normalize t computes the beta-normal form of t.  Since linear
-- lambda calculus is strongly normalizing, we can just pick an
-- arbitrary beta-redex, reduce it, and iterate until we reach a
-- (necessarily unique) normal form.
normalize :: ULT -> ULT
normalize t =
  let t' = step t in
  if t' == [] then t else normalize (head t')

-- test for beta equality
beta :: ULT -> ULT -> Bool
beta t u = alpha (normalize t) (normalize u)

-- computes the number of steps to reach a normal form.
distnf :: ULT -> Int
distnf t =
  let t' = step t in
  if t' == [] then 0 else 1 + distnf (head t')

-- return the list of eta-redexes in a term t.  An eta-redex is represented as
-- a triple (x,u,c), where L x (A u (V x)) is an eta-redex and t == plug c (L x (A u (V x))).
getEta :: ULT -> [(Int,ULT,ULT')]
getEta (V _) = []
getEta (A t u) = [(x1,u1,A'1 r1 u) | (x1,u1,r1) <- getEta t] ++
                 [(x2,u2,A'2 t r2) | (x2,u2,r2) <- getEta u]
getEta (L x t) = [(x,argfn t,V') | isApp t, argapp t == V x ] ++ 
                 [(x1,u1,L' x r1) | (x1,u1,r1) <- getEta t]

-- stepEta t computes all possible developments of t by one eta-redex
stepEta :: ULT -> [ULT]
stepEta t = do
  (x,t,c) <- getEta t
  return $ plug c t

-- normalize t computes the eta-normal form of t.
normalizeEta :: ULT -> ULT
normalizeEta t =
  let t' = stepEta t in
  if t' == [] then t else normalizeEta (head t')

-- test for eta equality
eta :: ULT -> ULT -> Bool
eta t u = alpha (normalizeEta t) (normalizeEta u)

-- test for beta-eta equality
betaEta :: ULT -> ULT -> Bool
betaEta t u = alpha (normalizeEta $ normalize t) (normalizeEta $ normalize u)

{-- TYPE INFERENCE AND CHECKING --}

data Type = TVar Int | TFn Type Type
  deriving (Show,Eq)
type TCtx = [(Int,Type)]

isTVar (TVar _) = True
isTVar (TFn _ _) = False

-- substitute a type for a type variable in a type
substT :: (Type,Int) -> Type -> Type
substT (t0,x) (TVar y) = if x == y then t0 else (TVar y)
substT (t0,x) (TFn t1 t2) = TFn (substT (t0,x) t1) (substT (t0,x) t2)

prettytvar :: Int -> String
prettytvar i = (["a","b","c","d","e","f"] ++ ["a" ++ show i | i <- [0..]]) !! i

prettyType :: Type -> String
prettyType (TVar x) = prettytvar x
prettyType (TFn a b) = paren (prettyType a ++ " -> " ++ prettyType b)

prettyTCtx :: TCtx -> String
prettyTCtx g = concat $ intersperse "," (map (\ (x,a) -> prettyvar x ++ ":" ++ prettyType a) g)

prettyConstraints :: [(Type,Type)] -> String
prettyConstraints [] = ""
prettyConstraints xi = "[" ++ intercalate ", " (map (\(s,t) -> prettyType s ++ " <= " ++ prettyType t) xi) ++ "]"

linearizeType :: Type -> [Int]
linearizeType (TVar x) = [x]
linearizeType (TFn a b) = linearizeType a ++ linearizeType b

linearizePos :: Type -> [Int]
linearizePos (TVar x) = [x]
linearizePos (TFn a b) = linearizePos b ++ linearizeNeg a

linearizeNeg :: Type -> [Int]
linearizeNeg (TVar x) = [x]
linearizeNeg (TFn a b) = linearizePos a ++ linearizeNeg b

newtype IntWrapped a = IW { runIW :: Int -> a }

class HasCounter a where
  fresh :: (Int -> a) -> a

instance HasCounter (IntWrapped a) where
  fresh f = IW (\n -> runIW (f n) (n+1))

-- linear lambda terms colored by explicit normal/neutral markers
data CLT_B = V_B Int | A_B CLT_B CLT_R | C_B CLT_R
  deriving (Eq,Show)
data CLT_R = L_R Int CLT_R | S_R CLT_B
  deriving (Eq,Show)

-- return the underlying lambda term of a colored term
forgetBlue :: CLT_B -> ULT
forgetRed :: CLT_R -> ULT

forgetBlue (V_B x) = V x
forgetBlue (A_B b r) = A (forgetBlue b) (forgetRed r)
forgetBlue (C_B r) = forgetRed r
forgetRed (L_R x r) = L x (forgetRed r)
forgetRed (S_R b) = forgetBlue b

-- compute the minimal coloring of a term
leastBlue :: ULT -> CLT_B
leastRed :: ULT -> CLT_R

leastBlue (V x) = V_B x
leastBlue (A t u) = A_B (leastBlue t) (leastRed u)
leastBlue t = C_B (leastRed t)
leastRed (L x t) = L_R x (leastRed t)
leastRed t = S_R (leastBlue t)

-- compute a "Horn" coloring of a ULT
hornBlue :: ULT -> CLT_B
hornRed :: ULT -> CLT_R

hornBlue (V x) = C_B (S_R (V_B x))
hornBlue (A t u) = A_B (hornBlue t) (S_R (C_B (hornRed u)))
hornBlue t = C_B (S_R (C_B (hornRed t)))
hornRed (L x t) = L_R x (hornRed t)
hornRed t = S_R (hornBlue t)

-- bidirectional type checking/inference operates on colored lambda terms

check :: HasCounter a => CLT_B -> Type -> (TCtx -> [(Type,Type)] -> a) -> a
infer :: HasCounter a => CLT_R -> (Type -> TCtx -> [(Type,Type)] -> a) -> a

check (V_B x) tau k = k [(x,tau)] []
check (A_B b r) tau k =
  infer r $ \sigma g2 xi2 ->
  check b (TFn sigma tau) $ \g1 xi1 ->
  k (g1 ++ g2) (xi1 ++ xi2)
check (C_B r) tau k =
  infer r $ \sigma g xi ->
  k g ((sigma,tau) : xi)
infer (L_R x r) k =
  infer r $ \tau g xi ->
  let (gx,g0) = partition (\p -> fst p == x) g in
  case gx of
    [(_,sigma)] -> k (TFn sigma tau) g0 xi
    _ -> error ("non-linear use of " ++ show x)
infer (S_R b) k =
  fresh $ \alpha ->
  check b (TVar alpha) $ \g xi ->
  k (TVar alpha) g xi

synth :: ULT -> ([Int],Type,TCtx,[(Type,Type)])
synth t = runIW (infer (leastRed t) (\a g xi -> IW (\n -> ([0..n-1],a,g,xi)))) 0

synthClosedNormal :: ULT -> Type
synthClosedNormal t =
  if freevars t /= [] then error "synthClosedNormal: not closed"
  else if not (isNormal t) then error "synthClosedNormal: not normal"
  else let (_,r,_,_) = synth t in r

synthHorn :: ULT -> ([Int],Type,TCtx,[(Type,Type)])
synthHorn t = runIW (infer (hornRed t) (\a g xi -> IW (\n -> ([0..n-1],a,g,xi)))) 0


inferPrintTerms :: [ULT] -> IO ()
inferPrintTerms ts = do
  mapM (\ t ->
         let (_,a,g,xi) = synth t in
         putStr (prettyTCtx g ++ " |- " ++ prettyULT t ++ " : " ++ prettyType a ++ " " ++ prettyConstraints xi ++ "\n")) ts
  return ()

inferHornPrintTerms :: [ULT] -> IO ()
inferHornPrintTerms ts = do
  mapM (\ t ->
         let (_,a,g,xi) = synthHorn t in
         putStr (prettyTCtx g ++ " |- " ++ prettyULT t ++ " : " ++ prettyType a ++ " " ++ prettyConstraints xi ++ "\n")) ts
  return ()

-- Bohm tree representation

data Bohm = Bohm [Int] Int [Bohm]
  deriving (Show,Eq)
           
normalToBohm :: ULT -> Bohm
normalToBohm t =
  let (vars,r) = unlambdas t in
  let (x,args) = unapps r [] in
  Bohm vars x (map normalToBohm args)

bohmToNormal :: Bohm -> ULT
bohmToNormal (Bohm vars x args) =
  foldr L (foldl (A) (V x) (map bohmToNormal args)) vars

-- canonical form of types

data TNF = TNF [TNF] Int
  deriving (Show,Eq)

tfn :: TNF -> TNF -> TNF
tfn a (TNF as r) = TNF (a:as) r

toTNF :: Type -> TNF
toTNF (TVar x) = TNF [] x
toTNF (TFn a b) = tfn (toTNF a) (toTNF b)

fromTNF :: TNF -> Type
fromTNF (TNF [] r) = TVar r
fromTNF (TNF (a:as) r) = TFn (fromTNF a) (fromTNF (TNF as r))

-- structural properties of lambda terms

-- compute underlying tree of applications, erasing lambdas
eraseLambdas :: ULT -> ULT
eraseLambdas (A t u) = A (eraseLambdas t) (eraseLambdas u)
eraseLambdas (V x) = (V x)
eraseLambdas (L x t) = eraseLambdas t

-- a "lambda skeleton" is a unary-binary tree, which represents the
-- underlying structure of lambdas and applications in a term but
-- ignores the matching structure between lambdas and variables.

ult2skel :: ULT -> ULTp ()
ult2skel (V _) = V ()
ult2skel (A t u) = A (ult2skel t) (ult2skel u)
ult2skel (L _ t) = L () (ult2skel t)

-- turn a lambda skeleton into a binary tree, where lambda nodes
-- are turned into a binary node with trivial right child if b is false
-- and trivial left child if b is true
skel2tree :: Bool -> ULTp () -> C.Tree
skel2tree b (V ()) = C.L
skel2tree b (A t u) = C.B (skel2tree b t) (skel2tree b u)
skel2tree b (L () t) = if b then C.B C.L (skel2tree b t)
                       else C.B (skel2tree b t) C.L

-- given a lambda skeleton, recover the unique planar lambda term with
-- that underlying skeleton, according to either the RL or LR planarity
-- convention (flag b).
skel2ULT_st :: Bool -> ULTp () -> State (Int,[Int]) ULT
skel2ULT_st b (V ()) = do
  (n,g) <- get
  if g == [] then error "skel2ULT_st: context empty at variable occurrence" else do
  let (x:g') = g
  () <- put (n,g')
  return $ V x
skel2ULT_st False (A t u) = do
  u' <- skel2ULT_st False u
  t' <- skel2ULT_st False t
  return $ A t' u'
skel2ULT_st True (A t u) = do
  t' <- skel2ULT_st True t
  u' <- skel2ULT_st True u
  return $ A t' u'
skel2ULT_st b (L () t) = do
  (n,g) <- get
  let x = n
  () <- put (n+1,x:g)
  t' <- skel2ULT_st b t
  return $ L x t'

skel2ULT :: Bool -> [Int] -> ULTp () -> ULT
skel2ULT b g t =
  let n = foldr max 0 g in
  case runState (skel2ULT_st b t) (n,g) of
    (t',(_,[])) -> t'
    (t',(_,_:_)) -> error "not all variables consumed!"

rl2lr :: ULT -> ULT
rl2lr t = skel2ULT True (freevars t) $ ult2skel t

lr2rl :: ULT -> ULT
lr2rl t = skel2ULT False (freevars t) $ ult2skel t
