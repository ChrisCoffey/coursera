module QuicksortCounting (
    quicksort,
    quicksort',
    quicksort2,
    quicksort2',
    quicksort2''
) where

import Data.List (sortBy)
import qualified Data.Vector as V


--Uses the first element as the pivot
quicksort2 :: Ord a => V.Vector a -> (V.Vector a, Int)
quicksort2 v
    | V.null v          = (v, 0)
    | V.length v == 1   = (v, 0) 
    | otherwise         = let
        pivot   = V.head v
        (l, r)  = partition' pivot v
        (lt, a) = quicksort2 l
        (gt, b) = quicksort2 r
        comps   = length v - 1
        in (lt V.++ V.singleton pivot V.++ gt, a + b + comps)

--Uses the last element as the pivot
quicksort2' :: Ord a => V.Vector a -> (V.Vector a, Int)
quicksort2' v 
    | V.null v          = (v, 0)
    | V.length v == 1   = (v, 0) 
    | otherwise         = let
        pivot   = V.last v
        v'      = v `V.update` (V.fromList [(0, pivot), (V.length v - 1, V.head v)]) --constant time swap to preserve partition' implementation
        (l, r)  = partition' pivot v'
        (lt, a) = quicksort2' l
        (gt, b) = quicksort2' r
        comps   = length v - 1
        in (lt V.++ V.singleton pivot V.++ gt, a + b + comps)

-- Uses the median of the head, tail, or midpoint
quicksort2'' :: (Ord a, Show a) => V.Vector a -> (V.Vector a, Int)
quicksort2'' v 
    | V.null v          = (v, 0)
    | V.length v == 1   = (v, 0)
    | otherwise         = let
        (pivot, idx)   = findPivot
        v'      = v `V.update` (V.fromList [(0, pivot), (idx, V.head v)]) --constant time swap to preserve partition' implementation
        (l, r)  = partition' pivot v'
        (lt, a) = quicksort2'' l
        (gt, b) = quicksort2'' r
        comps   = length v - 1
        in (lt V.++ V.singleton pivot V.++ gt, a + b + comps)
    where 
    middleIndex = if even $ V.length v then (V.length v `div` 2) -1 else (V.length v `div` 2 ) 
    findPivot = head . tail $ sortBy (\a b-> fst a `compare` fst b) [(V.head v, 0), 
                                    (V.last v, V.length v - 1),
                                    (v V.! middleIndex, middleIndex)]

partition' :: Ord a => a -> V.Vector a -> (V.Vector a, V.Vector a)
partition' pivot v = let
    (v', lowEdge , _) = foldl swap (v, 0, 0) v
    v'' = v' `V.update` (V.fromList [(lowEdge, V.head v'), (0, v' V.! lowEdge)])
    in (V.take lowEdge v'', V.drop (lowEdge + 1) v'')
    where 
    swap (vec, lowEdge, focus) _
        | vec V.! focus < pivot = let
            nextLowest = lowEdge + 1
            e  = vec V.! focus
            vec' = vec `V.update` (V.fromList [(focus, vec V.! nextLowest), 
                                               (nextLowest, e)])
            in (vec', nextLowest, focus + 1)
        | otherwise     = (vec, lowEdge, focus + 1)

quicksort :: Ord a => [a] -> ([a], Int)
quicksort [] = ([], 0)
quicksort [x] = ([x], 0)
quicksort (x:xs) = let
    (l, r) = partition x xs    
    comps  = length xs 
    (lt, a)= quicksort l
    (gt, b)= quicksort r 
    in (lt ++ [x] ++ gt, a + b + comps)
     
partition :: Ord a => a -> [a] -> ([a], [a])
partition p xs = (lt, gt)
    where
    lt = filter (< p) xs
    gt = filter (> p) xs

quicksort' :: Ord a => [a] -> ([a], Int)
quicksort' [] = ([],0)
quicksort' [x] = ([x], 0)
quicksort' xs = let
    x      = last xs 
    (l,r) = partition x (init xs)
    comps  = length xs - 1
    (lt, a)= quicksort' l
    (gt, b)= quicksort' r 
    in (lt ++ [x] ++ gt, a + b + comps)

