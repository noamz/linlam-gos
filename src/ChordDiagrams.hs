module ChordDiagrams where

import Data.List

import Bijections
import Catalan

-- test whether an involution is indecomposable
isIndecompInv :: Perm -> Bool
isIndecompInv p =
  let edges = nub $ map (\ds -> let (d,d') = (head ds,head (tail ds)) in (min d d',max d d')) (permToCycles p) in
  let incident (d1,d1') (d2,d2') =
        (d1 < d2 && d2 < d1') || (d2 < d1 && d1 < d2') in
  let component = gtrans (\e -> filter (incident e) edges) [head edges] [] in
  sort component == sort edges

-- [length $ filter isIndecompInv (involute [1..2*n]) | n <- [1..]] == [1,2,10,74,706,8162,...] == A000698

-- test whether an involution is connected
isConnectedInv :: Perm -> Bool
isConnectedInv p =
  let edges = nub $ map (\ds -> let (d,d') = (head ds,head (tail ds)) in (min d d',max d d')) (permToCycles p) in
  let crossing (d1,d1') (d2,d2') =
        (d1 < d2 && d2 < d1' && d1' < d2') ||
        (d2 < d1 && d1 < d2' && d2' < d1') in
  let component = gtrans (\e -> filter (crossing e) edges) [head edges] [] in
  sort component == sort edges

-- [length $ filter isConnectedInv (involute [1..2*n]) | n <- [1..]] == [1,1,4,27,248,2830,...] == A000699

isTerminal :: Bij -> (Int,Int) -> Bool
isTerminal p c =
  let edges = nub $ map (\ds -> let (d,d') = (head ds,head (tail ds)) in (min d d',max d d')) (permToCycles p) in
  let crossing (d1,d1') (d2,d2') =
        (d1 < d2 && d2 < d1' && d1' < d2') ||
        (d2 < d1 && d1 < d2' && d2' < d1') in
  all (\c' -> not (crossing c' c) || snd c' <= snd c) edges

-- naively compute the list of terminal chords in the intersection order of a chord diagram
terminalChords :: Bij -> [(Int,Int)]
terminalChords p =
  let edges = nub $ map (\ds -> let (d,d') = (head ds,head (tail ds)) in (min d d',max d d')) (permToCycles p) in
  filter (isTerminal p) edges

-- [length $ (filter (\t -> length (terminalChords t) == 1) ts) | n <- [1..], let ts = filter isConnectedInv (involute [1..2*n])] == [1,1,3,15,105,...]

-- [length $ (filter (\t -> length (terminalChords t) == 2) ts) | n <- [1..], let ts = filter isConnectedInv (involute [1..2*n])] == [0,0,1,11,116,1344,...]

-- [length $ (filter (\t -> length (terminalChords t) == 3) ts) | n <- [1..], let ts = filter isConnectedInv (involute [1..2*n])] == [0,0,0,1,26,490,...]

-- list arcs which cross a given chord in a chord diagram
arcCrossings :: Bij -> (Int,Int) -> [(Int,Int)]
arcCrossings p (d,d') =
  let edges = nub $ map (\ds -> let (d,d') = (head ds,head (tail ds)) in (min d d',max d d')) (permToCycles p) in
  let crossing (d1,d1') (d2,d2') =
        (d1 < d2 && d2 < d1' && d1' < d2') ||
        (d2 < d1 && d1 < d2' && d2' < d1') in
  filter (crossing (min d d',max d d')) edges

has3Crossing :: Bij -> Bool
has3Crossing p =
  let edges = nub $ map (\ds -> let (d,d') = (head ds,head (tail ds)) in (min d d',max d d')) (permToCycles p) in
  flip any edges $ \c1 ->
  flip any (arcCrossings p c1) $ \c2 ->
  flip any (arcCrossings p c2) $ \c3 ->
  elem c3 (arcCrossings p c1)

-- [length $ filter (not . has3Crossing) $ filter isConnectedInv (involute [1..2*n]) | n <- [1..]] == [1,1,3,14,82,...]
-- [length $ filter (not . has3Crossing) $ filter isIndecompInv (involute [1..2*n]) | n <- [1..]] == [1,2,9,55,400,...]

-- Karen's forbidden configuration
hasKYForbidden :: Bij -> Bool
hasKYForbidden p =
  let crossing (d1,d1') (d2,d2') =
        (d1 < d2 && d2 < d1' && d1' < d2') ||
        (d2 < d1 && d1 < d2' && d2' < d1') in
  let edges = nub $ map (\ds -> let (d,d') = (head ds,head (tail ds)) in (min d d',max d d')) (permToCycles p) in
  flip any edges $ \c1 ->
  flip any (arcCrossings p c1) $ \c2 ->
  let blob = gtrans (\e -> filter (\(d,d') -> d > fst c1 && d > fst c2) $ filter (crossing e) (edges \\ [c1])) [c2] [] in
  let blob' = gtrans (\e -> filter (\(d,d') -> d > fst c1 && d > fst c2) $ filter (crossing e) (edges \\ [c2])) [c1] [] in
  intersect blob blob' /= []

-- compute the list of left-to-right maximal chords of a chord diagram
l2rmaximalChords :: Bij -> [(Int,Int)]
l2rmaximalChords p =
  let edges = nub $ map (\ds -> let (d,d') = (head ds,head (tail ds)) in (min d d',max d d')) (permToCycles p) in
  [(i,j) | (i,j) <- edges, all (\(k,l) -> not (k < i) || l < j) edges]

-- [length $ (filter (\t -> length (l2rmaximalChords t) == 1) ts) | n <- [1..], let ts = filter isIndecompInv (involute [1..2*n])] == [1,1,3,15,105,945,...]
-- [length $ (filter (\t -> length (l2rmaximalChords t) == 2) ts) | n <- [1..], let ts = filter isIndecompInv (involute [1..2*n])] == [0,1,5,32,260,2589,...]
-- [length $ (filter (\t -> length (l2rmaximalChords t) == 3) ts) | n <- [1..], let ts = filter isIndecompInv (involute [1..2*n])] == [0,0,2,22,234,2750,...]

-- an involution on indecomposable diagrams sending terminal chords to top chords (found by Matthias, simplified by Julien)
phiinvol :: Arcs -> Arcs
phiinvol [] = []
phiinvol p =
  let (pre, D x:post) = span isup p in
  let p' = (pre \\ [U x]) ++ post in
  let phip' = phiinvol p' in
  let Just i = findIndex (== U x) pre in
  let j = length pre in
  let phip'' = let (a,b) = splitAt (j-i-1) phip' in a ++ U x : b in
  let (pre',post') = splitAt j phip'' in
  pre' ++ D x : post'
    
phiinvol' :: Bij -> Bij
phiinvol' = arcs2inv . phiinvol . inv2arcs

