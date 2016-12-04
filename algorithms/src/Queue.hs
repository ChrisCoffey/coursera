module Queue (Queue(..))where

class Queue q where
    makeEmpty :: q a

    push :: a -> q a -> q a
    pop :: q a -> (q a, Maybe a)
