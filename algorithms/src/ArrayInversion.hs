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
