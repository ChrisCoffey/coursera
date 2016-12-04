module Main where

import qualified Data.Map as M
import qualified Dijkstra as D
import SCCs

import Data.List (sortBy, nub, intercalate)

main :: IO ()
main = runDijkstra

runDijkstra :: IO ()
runDijkstra = do
    g <- D.makeGraph "data/DijkstraData.txt"
    let paths = D.shortestPaths g
    let res = (paths M.!) <$> [7,37,59,82,99,115,133,165,188,197]
    print . intercalate "," . fmap show $ res

runSCCs :: IO ()
runSCCs = do
    lines <- lines <$> readFile "data/SCC.txt"
    print "Loading graph"
    g <- pure $ makeGraph lines
    print "Graph Loaded"
    print "Finding SCCs"
    sccs <- pure $ findSCCs g
    print "SCCs Found"
    print "Preparing Output"
    lengths <- pure (sortBy s $ M.toList sccs)
    print . show $ take 100 (length . snd <$> lengths)
    where 
    s l r = (length $ snd r) `compare` (length $ snd l)
