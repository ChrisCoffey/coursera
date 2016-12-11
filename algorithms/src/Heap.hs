module Heap (
    Heap (..),
    MaxHeap (..)
) where


class Heap h where
    isEmpty :: Ord a => h a -> Bool
    getMin :: Ord a => h a -> a
    deleteMin :: Ord a => h a -> h a
    insert :: Ord a => h a -> a -> h a
    merge :: Ord a => h a -> h a -> h a
    makeEmpty :: Ord a => h a


class MaxHeap h where
    isEmptyMax :: Ord a => h a -> Bool
    getMax :: Ord a => h a -> a
    deleteMax :: Ord a => h a -> h a
    insertMax :: Ord a => h a -> a -> h a
    mergeMax :: Ord a => h a -> h a -> h a
    makeEmptyMax :: Ord a => h a
