module Main where

import SCCs
import Data.Map
import Data.List (sortBy, nub)

main :: IO ()
main = runSCCs

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
    lengths <- pure (sortBy s $ toList sccs)
    print . show $ take 100 (length . snd <$> lengths)
    where 
    s l r = (length $ snd r) `compare` (length $ snd l)
