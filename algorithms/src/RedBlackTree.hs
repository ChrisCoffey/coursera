module RedBlackTree where

import SearchTree

data Color = Red | Black
    deriving (Show, Ord, Bounded, Enum, Eq)

data RBTree a 
    = Empty 
    | Node {
        color :: Color, 
        elem :: a,
        leftChild :: RBTree a,
        rightChild :: RBTree a
        }
    deriving (Show, Eq)

balance :: Ord a => Color -> RBTree a -> a -> RBTree a -> RBTree a
balance Black (Node Red y (Node Red x a b) c) z d = Node Red y 
                                                        (Node Black x a b)  
                                                        (Node Black z c d)
balance Black (Node Red x a (Node Red y b c)) z d = Node Red y
                                                        (Node Black x a b)  
                                                        (Node Black z c d)
balance Black a x (Node Red z (Node Red y b c) d) = Node Red y
                                                        (Node Black x a b)
                                                        (Node Black z c d)
balance Black a x (Node Red y b (Node red z c d)) = Node Red y 
                                                        (Node Black x a b)
                                                        (Node Black z c d)
balance c l x r = Node c x l r

size :: RBTree a -> Int
size Empty = 0
size (Node _ _ l r) = 1 + size l + size r


instance SearchTree RBTree where
    memberOfTree Empty _ = False
    memberOfTree (Node _ v a b) x
        | x < v = memberOfTree a x
        | x > v = memberOfTree b x
        | otherwise = True
    
    root Empty = Nothing
    root (Node _ v _ _) = Just v

    addToTree t x = res {color = Black}
        where
        go Empty = Node Red x Empty Empty
        go t'@(Node c y a b)
            | x < y = balance c (go a) y b
            | x > y = balance c a y (go b)
            | otherwise = t'
        res = go t

    removeFromTree = undefined
