module Main where

import MinCuts

main :: IO ()
main = do
    lines <- lines <$> readFile "data/MinCuts.txt"
    let ns = fmap read .  words <$> lines :: [[Int]]
    let g = makeGraph ns
    print . show . minimum =<< minCutNtrials 40 g
