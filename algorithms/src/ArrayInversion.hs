module ArrayInversion (
    countInversions,
    merge,
    mergesort,
    SortedList(..)
) where

type ListCount = ([Int], Int)

addToLC :: Int -> ListCount -> ListCount
addToLC x (xs, n) = (x:xs, n)

incLC :: Int -> ListCount -> ListCount
incLC amt (xs, n) = (xs, n + amt)

countInversions :: [Int] -> Int
countInversions = snd . countInversions'

countInversions' :: [Int] -> ListCount
countInversions' [a] = ([a], 0)
countInversions' xs = merge l r
    where
    l = take (length xs `div` 2) xs -- knowingly inefficient
    r = drop (length xs `div` 2) xs

merge :: [Int] -> [Int] -> ListCount
merge as bs = combine l r
    where
    l = countInversions' as
    r = countInversions' bs
    combine :: ListCount -> ListCount -> ListCount
    combine ([], x) b = incLC x b
    combine a ([], x) = incLC x a
    combine (a:a', n) (b:b', m) 
        | b < a = incLC (length (a:a')) . addToLC b $ combine (a:a', n) (b', m)
        | otherwise = addToLC a $ combine (a', n) (b:b', m)

data SortedList = SortedList {
    inversionCount :: Int,
    list :: [Int]
} deriving (Show) 

--      first   list        accumulator
packm :: Int -> SortedList -> Int -> SortedList
packm x (SortedList count xs) add =  SortedList (count + add) (x:xs)

merge2 :: [Int] -> [Int] -> SortedList
merge2 [] xs = SortedList 0 xs
merge2 xs [] = SortedList 0 xs
merge2 xlist@(x:xs) ylist@(y:ys)
    | x < y = packm x (merge2 xs ylist) 0
    | otherwise = packm y (merge2 xlist ys) $ length xlist

countAndMerge :: SortedList -> SortedList -> SortedList
countAndMerge (SortedList lcount lxs) (SortedList rcount rxs) =
    let merged = merge2 lxs rxs
    in SortedList (lcount + rcount + inversionCount merged) $ list merged

mergesort :: [Int] -> SortedList
mergesort [] = SortedList 0 []
mergesort [x] = SortedList 0 [x]
mergesort xs =
    let leftsorted = mergesort $ take halfElements xs
        rightsorted = mergesort $ drop halfElements xs
    in countAndMerge leftsorted rightsorted
    where halfElements = length xs `div` 2
