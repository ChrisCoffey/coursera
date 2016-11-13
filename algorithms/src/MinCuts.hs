module MinCuts where

import qualified Data.Map.Lazy as M
import Data.List (union, nub)
import System.Random

type Graph = M.Map Vertex [Vertex]
type Vertex = Int

makeGraph :: [[Vertex]] -> Graph
makeGraph adjacencyList = M.fromList $ (\(x:xs)-> (x,xs)) <$> adjacencyList

edges :: Graph -> Vertex -> [Vertex]
edges = (M.!)

vertices :: Graph -> [Vertex]
vertices = M.keys

updateEdges :: Graph -> Vertex -> [Vertex] -> Graph
updateEdges g v e = M.adjust (const e) v g

removeVertex :: Vertex -> Graph -> Graph
removeVertex = M.delete

{- The combination algorithm takes both lists of edges & combines them together, removing an self-edges. 
 - Next, it removes `b` from the graph & updates `a` to the set of combined edges. Finally, it walks the graph
 - and replaces any reference to `b` with `a`
 -}
collapseVertices :: Int -> Int -> Graph -> Graph
collapseVertices a b g = let
    edgesA = g `edges` a
    edgesB = g `edges` b
    combinedEdges = filter (\v-> v /= a && v /= b) $  edgesA ++ edgesB
    g' = updateEdges (removeVertex b g) a combinedEdges
    in fmap (\v-> if v == b then a else v) <$> g'

randomContraction :: RandomGen r => r -> Graph -> (Graph, r)
randomContraction r g = let  
    -- Select a random edge
    vs = vertices g 
    n = length vs
    (i, r') = randomR (0, n-1) r
    v = vs !! i
    m = length $ edges g v
    (i', r'') = randomR (0, m-1) r'
    u = (edges g v) !! i'
    -- contract the two vertices on our edge
    g' = collapseVertices v u g
    -- return the resulting graph
    in (g', r'')

minCut :: RandomGen r => r -> Graph -> Graph
minCut r g 
    | (length $ vertices g) == 2 = g
    | otherwise = let
        (g', r') = randomContraction r g
        in minCut r' g'
        
findMinCut :: Graph -> IO Int
findMinCut g = do
    r <- newStdGen
    let g' = minCut r g
    let v = head $ vertices  g'
    pure . length $ edges g' v


minCutNtrials :: Int -> Graph -> IO [Int]
minCutNtrials n g = traverse (const $ findMinCut g) [0..n]
