module Catalan where

data Catalan = L | B Catalan Catalan
  deriving (Show,Eq)

size :: Catalan -> Int
size L = 0
size (B t1 t2) = 1 + size t1 + size t2

catalan :: Int -> [Catalan]
catalan 0 = [L]
catalan n = [B t1 t2 | i <- [0..n-1], t1 <- catalan i, t2 <- catalan (n-1-i)]

dyck2cat_cps :: Eq a => a -> [a] -> ([a] -> Catalan -> r) -> r
dyck2cat_cps x (y:w) k = if x == y then k w L
                         else dyck2cat_cps y w $ \w' t1 -> dyck2cat_cps x w' $ \w'' t2 -> k w'' (B t1 t2)
dyck2cat_cps x [] k = error "not a dyck word"

dyck2cat :: Eq a => [a] -> Catalan
dyck2cat (x:xs) = dyck2cat_cps x xs (\w' t -> if w' == [] then t else error "not a closed dyck word")
