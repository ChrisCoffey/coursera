module PrimsMST (
    mstCost   
) where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Data.PSQueue as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

newtype Vertex = Vertx Int deriving (Show, Eq)
data Edge = Edge {s :: Vertex, e :: Vertex, w:: Int} deriving (Eq, Show)

type Graph = M.Map Int Edge

mstCost :: 
    String ->
    IO Int
mstCost = undefined

loadGraph :: IO Graph
loadGraph = undefined
