module SearchTree (
    SearchTree(..)
) where

class SearchTree t where
    addToTree :: Ord a => t a -> a -> t a
    removeFromTree :: Ord a => t a -> a -> t a
    root :: t a -> Maybe a
    memberOfTree :: Ord a => t a -> a -> Bool
    
