module Main where

import QuicksortCounting

main :: IO ()
main = do
    lines <- lines <$> readFile "data/QuicksortCounting.txt"
    let ns = read <$> lines :: [Int]
    print $ quicksort' ns

