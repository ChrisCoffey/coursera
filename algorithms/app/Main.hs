module Main where

import ArrayInversion

main :: IO ()
main = do
    lines <- lines <$> readFile "data/ArrayInversions.txt"
    let ns = read <$> lines :: [Int]
    print $ countInversions ns

