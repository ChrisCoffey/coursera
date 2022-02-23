{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module MaxSpaceKCluster where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Heap as H
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (pack, unpack, splitOn)

import Debug.Trace

type DistanceFunction = M.Map Int (M.Map Int Int)
type Edges = H.MinPrioHeap Int (Int, Int)

data UnionNode = UnionNode {rank :: Int, root :: Int} deriving Show
type UnionFind = M.Map Int UnionNode -- Padded to be 1 indexed

uFind :: 
    Int -> -- Key to find cluster for
    UnionFind -> --unionfind
    Int  -- cluster
uFind key uf
    | key == root (uf M.! key) = key
    | otherwise             =  uFind (root $ uf M.! key) uf

uUnion ::
    Int ->
    Int ->
    UnionFind ->
    UnionFind
uUnion a b uf = let
    a' = uf M.! a 
    b' = uf M.! b
    in if
        | rank a' > rank b' -> M.insert b (b' {root=a}) uf
        | rank a' < rank b' -> M.insert a (a' {root=b}) uf
        | otherwise -> let 
            uf' = M.insert b (b' {root=a}) uf
            in M.insert a (a' {rank= rank a' + 1}) uf'

loadEdges :: 
    String -> 
    IO Edges
loadEdges path = do  
    ints <- fmap ( fmap parseLine . lines) $  readFile path
    pure $ H.fromList [(d,(a,b))| (b,a,d) <- ints]

loadEdgeFunction :: 
    String -> 
    IO DistanceFunction
loadEdgeFunction path = do
    trips <- fmap parseLine . lines <$> readFile path
    pure $ foldr addToF M.empty trips 
    where
    addToF (to, from, dist) =  M.insertWith M.union to (M.singleton from dist) 

parseLine :: 
    String -> 
    (Int, Int, Int)
parseLine =  toTuple . splitOn " " . pack
    where
    toTuple [a,b,c] = (read $ unpack a, read $ unpack b, read $ unpack c)


third :: (a,b,c) -> c
third (a,b,c) = c

first (a,b,c) = a
second (a,b,c) = b

--TODO with clear head: debug this
singleLinkCluster :: 
    Int -> -- k
    UnionFind ->
    Edges -> -- sorted edges
    Int -- Max-spacing
singleLinkCluster k uf edges = 
    go uf edges (M.size uf)
    where
    go :: UnionFind -> Edges -> Int -> Int
    go unionfind es clusterCount
        | H.null es = -1
        | uFind a unionfind == uFind b unionfind = go unionfind edges' clusterCount -- if same cluster, ignore & continue
        | clusterCount == k = dist -- spacing on remaining edge
        | otherwise = go (uUnion a b unionfind) edges' (clusterCount -1)
        where
        Just ((dist, (a,b)), edges') = H.view es
           
distinctRoots ::
    UnionFind ->
    Int
distinctRoots uf = S.size $ S.fromList [uFind i uf | i <- [1..M.size uf]]

-- To implement K-Clustering (K = cluster count) need the following
-- 1: A Union-Find of all points
-- 2: A list of all points
--
-- Algorithm:
--  PreProcess- 
--      sort all edges by distance
--      initialize union find with all 500 elements set to themselves
--
--  while (cluster-count > 4) 
--      get points a,b w/ smallest distance from point list
--      if a,b in different clusters
--          union the clusters
--
--  return the space between the next node in the list
