module QuicksortCounting (
    quicksort,
    quicksort'
) where

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

