module SCCs where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

import Debug.Trace

type Graph = M.Map Vertex [Vertex]
type Vertex = String

data LabeledGraph = LabeledGraph {
    seen :: S.Set Vertex,
    finishingTimes :: M.Map Int Vertex,
    graph :: Graph,
    time :: Int
} deriving Show

data SccForest = SccForest {
    seenScc :: S.Set Vertex,
    sccs :: Graph,
    graphScc :: Graph,
    leader :: Vertex
} deriving Show

edges :: Vertex -> Graph -> [Vertex]
edges v g = fromMaybe [] $ M.lookup v g

unseen :: Vertex -> LabeledGraph -> Bool
unseen v lg = not . S.member v $ seen lg

vertices :: Graph -> [Vertex]
vertices = M.keys

em :: M.Map Vertex [Vertex]
em = M.empty

makeGraph :: [String] -> Graph
makeGraph = foldl f em
    where 
    f :: M.Map Vertex [Vertex] -> String -> M.Map Vertex [Vertex]
    f acc edge = let
        (t, h) = span (/= ' ') edge
        in M.insertWith (++) t [takeWhile (/= ' ') $ tail h] acc

reverseEdges :: Graph -> Graph
reverseEdges = M.foldrWithKey f em
    where
    f tail edges acc = foldl (\a e -> M.insertWith (++) e [tail] a) acc edges

dfs :: Vertex -> LabeledGraph -> LabeledGraph
dfs v g = let
    es = edges v $ graph g
    toSearch = filter (`unseen` g) es
    g' = f toSearch g {seen = S.insert v (seen g)}
    in g' {time= time g' + 1, finishingTimes= M.insert (time g') v (finishingTimes g')}
    where 
    f es g' = foldl (\acc e-> if unseen e acc then dfs e acc {seen = S.insert e (seen acc)} else acc) g' es

dfsLoop :: LabeledGraph -> LabeledGraph
dfsLoop g = let
    vs = vertices $ graph g
    in foldl f g vs
    where
    f g' v = if unseen v g' then dfs v g' else g'
   
findSCCs :: Graph -> Graph
findSCCs g = let
    lg = LabeledGraph {seen = S.empty, 
                       finishingTimes = M.empty, 
                       graph = reverseEdges g, 
                       time = 1}
    lg' = dfsLoop lg
    r = [1..len (finishingTimes lg')]
    sccf = SccForest {seenScc = S.empty, sccs= M.empty, graphScc = g, leader= ""}
    sccf' = foldl (f (finishingTimes lg')) sccf (reverse r)
    in sccs sccf'
    where
    len m = fst $ M.findMax m
    f :: M.Map Int Vertex -> SccForest -> Int -> SccForest
    f times sf n = let
        v = times M.! n
        in if S.member v (seenScc sf) then sf else dfsAccum v sf {leader= v}

dfsAccum :: Vertex -> SccForest -> SccForest
dfsAccum v g = let
    es = edges v $ graphScc g
    toSearch = filter (`new` g) es
    g' = f toSearch g {seenScc = S.insert v (seenScc g)}
    in g' {sccs = M.insertWith (++) (leader g') [v] (sccs g')}
    where
    new v sf = not . S.member v $ seenScc sf
    f es g' = foldl (\acc v -> if new v acc then dfsAccum v acc else acc ) g' es
