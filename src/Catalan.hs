module Catalan where

data Catalan = L | B Catalan Catalan
  deriving (Show,Eq)

nodes :: Catalan -> Int
nodes L = 0
nodes (B t1 t2) = 1 + nodes t1 + nodes t2

leaves :: Catalan -> Int
leaves L = 1
leaves (B t1 t2) = leaves t1 + leaves t2

catalan :: Int -> [Catalan]
catalan 0 = [L]
catalan n = [B t1 t2 | i <- [0..n-1], t1 <- catalan i, t2 <- catalan (n-1-i)]

dyck2cat_cps :: Eq a => a -> [a] -> ([a] -> Catalan -> r) -> r
dyck2cat_cps x (y:w) k = if x == y then k w L
                         else dyck2cat_cps y w $ \w' t1 -> dyck2cat_cps x w' $ \w'' t2 -> k w'' (B t1 t2)
dyck2cat_cps x [] k = error "not a dyck word"

dyck2cat :: Eq a => [a] -> Catalan
dyck2cat (x:xs) = dyck2cat_cps x xs (\w' t -> if w' == [] then t else error "not a closed dyck word")

dycks2cat :: Eq a => [a] -> Catalan
dycks2cat [] = L
dycks2cat (x:xs) = dyck2cat_cps x xs (\w' t -> B t (dycks2cat w'))
