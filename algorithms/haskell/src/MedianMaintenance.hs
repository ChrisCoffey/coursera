{-# LANGUAGE MultiWayIf #-}
 
module MedianMaintenance where

import Heap
import LeftistHeap


import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Intro as I
import Debug.Trace

data SizeAnnotatedHeap a = SizeAnnotatedHeap {
    size :: Int,
    heap :: LeftistHeap a
} deriving (Show)

instance Heap SizeAnnotatedHeap where
    isEmpty h = isEmpty $ heap h
    getMin h = getMin $ heap h
    deleteMin h = h {size = (size h) -1, heap = h'}
        where
        h' = deleteMin $ heap h
    insert h x = h {size = (size h) + 1, heap = h'}
        where
        h' = insert (heap h) x
    merge l r = SizeAnnotatedHeap s' h'
        where
        s' = size l + size r
        h' = merge (heap l) (heap r)
    makeEmpty = SizeAnnotatedHeap 0 makeEmpty 

instance MaxHeap SizeAnnotatedHeap where
    isEmptyMax h = isEmptyMax $ heap h
    getMax h = getMax $ heap h
    deleteMax h = h {size = size h - 1, heap = deleteMax $ heap h}
    insertMax h x = h {size = size h + 1, heap = insertMax (heap h) x}
    mergeMax l r = SizeAnnotatedHeap s' h'
        where
        s' = size l + size r
        h' = mergeMax (heap l) (heap r)
    makeEmptyMax = SizeAnnotatedHeap 0 makeEmptyMax 

rawData :: FilePath -> IO (V.Vector Int)
rawData path = V.fromList . fmap (\x-> read x :: Int) . lines <$> readFile path

computeMedians :: 
    [Int] -> 
    [Int]
computeMedians raw = let
    (res, _, _) = foldl f ([], makeEmptyMax, makeEmpty) raw
    in res
    where
    f (medians, lower, higher) x = let
        (lower', higher') = if -- Add to a heap
            | shouldAddToL x higher -> (insertMax lower x, higher)
            | otherwise -> (lower, insert higher x)
        (minCheckL, minCheckH) = if -- If unbalanced, move lower to higher
            | lowerTooLarge lower' higher' -> let
                lm = getMax lower'
                in (deleteMax lower', insert higher' lm)
            | otherwise -> (lower', higher')
        (maxCheckL, maxCheckH) = if -- If that unbalances too much, we moved median
            | higherLarger minCheckL minCheckH -> let
                hm = getMin minCheckH
                in (insertMax minCheckL hm, deleteMin minCheckH)
            | otherwise -> (minCheckL, minCheckH)
        in (getMax maxCheckL:medians, maxCheckL, maxCheckH)
    shouldAddToL x higher = not (isEmpty higher) && x < getMin higher 
    lowerTooLarge l h = size l - size h > 1
    higherLarger l h = size h > size l


--
-- The easy way to solve
--
median :: V.Vector Int -> Int
median xs = let 
    k = V.length xs
    srted = V.modify I.sort xs
    in if even k
       then srted V.! ((k `div` 2) -1)
       else srted V.! (k`div` 2)

inits :: V.Vector Int -> [V.Vector Int]
inits v = go v []
    where
    go :: V.Vector Int -> [V.Vector Int] -> [V.Vector Int]
    go xs acc
        | V.null xs = acc
        | otherwise = go (V.init xs) (xs:acc)

simpleAnswer ::  IO Int
simpleAnswer = do
    r <- rawData "data/MedianMaintenance.txt"
    let ms = median <$> inits r
    pure $ sum ms
