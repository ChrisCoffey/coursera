module LQueue (LQueue) where

import Queue

data LQueue a = LQueue {front :: [a], back :: [a]}

instance Queue LQueue where
    makeEmpty = LQueue [] []

    push a (LQueue f b) = LQueue {front = f, back= a:b}
    pop q@(LQueue [] []) = (q, Nothing)
    pop (LQueue [] b) = let
        (a:as) = reverse b
        in (LQueue {front = as, back = []}, Just a)
    pop (LQueue (a:rest) b) = (LQueue {front = rest, back = b}, Just a)
