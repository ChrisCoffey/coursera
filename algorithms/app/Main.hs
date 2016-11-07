module Main where

import QuicksortCounting

import qualified Data.Vector as V

main :: IO ()
main = do
    lines <- lines <$> readFile "data/QuicksortCounting.txt"
    let ns = read <$> lines :: [Int]
    print . snd $ quicksort2 (V.fromList ns)
    print . snd $ quicksort2' (V.fromList ns)
    print . snd $ quicksort2'' (V.fromList ns)

