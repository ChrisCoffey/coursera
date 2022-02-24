{-# LANGUAGE MultiWayIf #-}

module PrimsMST (
    mstCost   
) where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Data.PSQueue as H

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

newtype Vertex = Vertex Int deriving (Show, Eq, Ord)
data Edge = Edge {s :: Vertex, e :: Vertex, w:: Int} deriving (Eq, Show, Ord)

type Graph = M.Map Vertex [Edge]
type PHeap = H.PSQ Int Int

mstCost :: 
    String ->
    IO Int
mstCost path = do
    g <- loadGraph path
    let mst = heapMST g
    let cost = foldr (\e x-> x + w e) 0 mst
    pure cost

naiveMST :: Graph -> S.Set Edge
naiveMST g =  undefined
    --foreach node in V-X
        --foreach edge not already in T
            -- if edge has smallest cost & introduces a vertex not in V
            -- then add e to T & v to X
    -- stop when X == V

heapMST :: Graph -> S.Set Edge
heapMST g = step S.empty (S.singleton (Vertex 1)) initialHeap
    where
    initialHeap = H.fromList . fmap (\e -> e H.:-> w e) $ g M.! Vertex 1
    step t x heap
        | H.null heap = t
        | otherwise = let
            Just (edge H.:-> weight, heap') = H.minView heap
            in if 
                | S.member (s edge) x && S.member (e edge) x -> step t x heap'
                | S.member (s edge) x -> let
                    heapNewEdges = foldr (\e h'-> H.insert e (w e) h') heap' $ g M.! e edge
                    in step (S.insert edge t) (S.insert (e edge) x) heapNewEdges
                | S.member (e edge) x-> let
                    heapNewEdges = foldr (\e h'-> H.insert e (w e) h') heap' $ g M.! s edge
                    in step (S.insert edge t) (S.insert (s edge) x) heapNewEdges
                | otherwise -> error "how did we get here"

-- Load each edge in order, adding the edge to the given edge list for a start vertex & end vertex
loadGraph ::
    FilePath ->
    IO Graph
loadGraph path = foldl addEdge M.empty. lines <$> readFile path
    where
    addEdge :: Graph -> String -> Graph
    addEdge g raw = 
        case parse parseEdge "edge parser" raw of
            Left e -> error (show e)
            Right edge -> let 
                g' = M.insertWith (++) (e edge) [edge] g
                in M.insertWith (++) (s edge) [edge] g'

parseEdge :: Parser Edge
parseEdge = do
    s <- Vertex <$> n
    char ' '
    e <- Vertex <$> n
    char ' '
    w <- n
    pure Edge {s = s, e = e, w = w}
    where n = read <$> many1 (char '-' <|> digit)
