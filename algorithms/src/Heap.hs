module Heap (
    Heap(..)
) where


class Heap h where
    isEmpty :: Ord a => h a -> Bool
    getMin :: Ord a => h a -> a
    deleteMin :: Ord a => h a -> h a
    insert :: Ord a => h a -> a -> h a
    merge :: Ord a => h a -> h a -> h a
    makeEmpty :: Ord a => h a

