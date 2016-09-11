module Scott1980 where

import Data.List

import Lambda

compose :: ULT -> ULT -> ULT
compose t u = L 0 (A t (A u (V 0)))

isIdempotent :: ULT -> Bool
isIdempotent t = beta (compose t t) t

idempotents :: [ULT] -> [ULT]
idempotents = filter isIdempotent

p0 :: ULT
p0 = L 0 $ L 1 $ A (V 0) (V 1)
-- isIdempotent p0 == True

p :: ULT
p = L 0 $ L 1 $ L 2 $ A (A (V 0) (V 1)) (V 2)
-- isIdempotent p == True

q :: ULT
q = L 0 $ L 1 $ A (V 0) (L 2 (A (V 1) (V 2)))
-- isIdempotent q == True

{-
inferPrintTerms [p,q]
 |- \x -> \y -> \z -> x(y)(z) : ((c -> (b -> a)) -> (c -> (b -> a))) 
 |- \x -> \y -> x(\z -> y(z)) : (((c -> b) -> a) -> ((c -> b) -> a))
-}

isMorphism :: ULT -> ULT -> ULT -> Bool
isMorphism f a b = beta (compose b (compose f a)) f

prod :: ULT -> ULT -> ULT
prod p q = L 0 $ L 1 $ apps 1 [A p (A (V 0) (L 2 $ L 3 $ V 2)),
                               A q (A (V 0) (L 2 $ L 3 $ V 3))]

-- isIdempotent (prod p q) == True

proj1 :: ULT -> ULT -> ULT
proj1 p q = L 0 $ A (A (prod p q) (V 0)) (L 2 $ L 3 $ V 2)
proj2 :: ULT -> ULT -> ULT
proj2 p q = L 0 $ A (A (prod p q) (V 0)) (L 2 $ L 3 $ V 3)

-- isMorphism (proj1 p q) (prod p q) p == True
-- isMorphism (proj2 p q) (prod p q) q == True

tensor :: ULT -> ULT -> ULT
tensor p q = L 0 $ L 1 $ L 2 $ apps 0 [A p (V 1), A q (V 2)]

-- isIdempotent (tensor p q) == True

sym :: ULT -> ULT -> ULT
sym p q = L 0 $ L 1 $ L 2 $ apps 0 [A p (V 2), A q (V 1)]

-- isMorphism (sym p q) (tensor p q) (tensor q p) == True
-- isMorphism (sym p q) (tensor p q) (tensor p q) == False

lolli :: ULT -> ULT -> ULT
lolli p q = L 0 $ L 1 $ A q (A (V 0) (A p (V 1)))

-- isIdempotent (lolli p q) == True

eval :: ULT -> ULT -> ULT
eval p q = compose q $ L 0 $ L 1 $ L 2 $ apps 0 [A p (V 1), A (lolli p q) (V 2)]

-- isMorphism (eval p q) (tensor p (lolli p q)) q == True

-- let props = idempotents $ allcNLT 4 in all id [isMorphism (eval p q) (tensor p (lolli p q)) q | p <- props, q <- props]
