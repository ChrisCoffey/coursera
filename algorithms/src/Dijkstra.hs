module Dijkstra (
 makeGraph,
 edges
) where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import Heap
import LeftistHeap

import Text.Parsec
import Text.Parsec.String

data Edge = Edge {start:: Int, end:: Int, weight:: Int}
    deriving (Show, Eq, Ord)
type Graph = M.Map Int [Edge]

edges :: Int -> Graph -> [Edge]
edges = flip (M.!)

makeGraph :: FilePath -> IO Graph
makeGraph path = do
    ls <- lines <$> readFile path
    let edges = foldl addVertex M.empty ls
    pure edges
    where
    addVertex :: Graph -> String -> Graph
    addVertex g line = case parse parseLine "lineParser" line of
        Left _ -> error ("Failed to parse line: " ++ line)
        Right es -> foldl f g es
                where f acc e = M.insertWith (++) (start e) [e] acc

parseLine :: Parser [Edge]
parseLine = do
    vertex <- n
    es <- many1 edge
    return $ toEdge vertex <$> es 
    where
    n = read <$> many1 digit
    edge :: Parser (Int, Int)
    edge = do
        char ' '
        e <- n
        char ','
        w <- n
        return (e, w)
    toEdge v (e, w) = Edge {start = v, end = e, weight = w}


