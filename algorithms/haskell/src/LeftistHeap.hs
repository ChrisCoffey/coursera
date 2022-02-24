module LeftistHeap (
    LeftistHeap,
    deleteArb
) where

import Heap

data LeftistHeap a = Empty | Node Int a (LeftistHeap a) (LeftistHeap a)
    deriving (Show)


rank :: LeftistHeap a -> Int
rank Empty = 0
rank (Node r _ _ _) = r

makeNode x l r = if rank l >= rank r
                 then Node (rank r + 1) x l r
                 else Node (rank l + 1) x r l

{-Not terribly fast -}
deleteArb :: (Ord a, Eq a) => a -> LeftistHeap a -> LeftistHeap a
deleteArb x h = let
    hs = drain h
    hs' = filter (/= x) hs
    in foldl insert makeEmpty hs'

drain ::  LeftistHeap a -> [a]
drain Empty = []
drain (Node _ x l r) = x:(drain l ++ drain r)

instance Heap LeftistHeap where
    makeEmpty = Empty
    
    isEmpty Empty = True
    isEmpty _ = False

    insert h x = merge (makeNode x Empty Empty) h
   
    merge h Empty = h
    merge Empty h = h
    merge h@(Node _ x l r) h'@(Node _ x' l' r') = 
        if x < x'
        then makeNode x l (merge r h')
        else makeNode x' l' (merge r' h)
    
    getMin Empty = error "Boom. Empty Heap has no min"
    getMin (Node _ m _ _) = m
    
    deleteMin Empty = error "Boom. Empty Heap has no min. Can't delete it"
    deleteMin (Node _ _ l r) = merge l r

instance MaxHeap LeftistHeap where
    makeEmptyMax = Empty
    isEmptyMax Empty = True
    isEmptyMax _ = False
    
    insertMax h x = mergeMax (makeNode x Empty Empty) h
    
    mergeMax h Empty = h
    mergeMax Empty h = h
    mergeMax h@(Node _ x l r) h'@(Node _ x' l' r') = 
        if x > x'
        then makeNode x l (mergeMax r h')
        else makeNode x' l' (mergeMax r' h)

    getMax Empty = error "No max in an empty heap"
    getMax (Node _ m _ _) = m

    deleteMax Empty = error "Can't delete max from an empty heap"
    deleteMax (Node _ _ l r) = mergeMax l r

