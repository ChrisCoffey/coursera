module Dijkstra (
 makeGraph,
 shortestPaths
) where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Queue as Q
import qualified Data.PSQueue as H

import LQueue

import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String

import Debug.Trace

data Edge = Edge {start:: Int, end:: Int, weight:: Int}
    deriving (Show, Eq, Ord)
data WeightedVertex = WeightedVertex {vId :: Int, vWeight :: Int}
    deriving (Show, Eq)

type Graph = M.Map Int [Edge]

instance Ord WeightedVertex where
    compare (WeightedVertex _ l) (WeightedVertex _ r) = l `compare` r

type PHeap = H.PSQ Int Int

edges :: Int -> Graph -> [Edge]
edges = flip (M.!)

edgeToWeightedVertex :: Edge -> WeightedVertex
edgeToWeightedVertex (Edge _ e w) = WeightedVertex {vId = e, vWeight = w}

shortestPaths :: Graph -> M.Map Int Int
shortestPaths graph = let
    graphSize = M.size graph
    is = [2..graphSize]
    v = S.fromList is
    heap = H.fromList $ (H.:-> 1000000) <$> is
    heap' = updateHeap 0 (edges 1 graph) heap
    in findAllPaths graph S.empty heap' M.empty

findAllPaths :: Graph -> S.Set Int -> PHeap -> M.Map Int Int -> M.Map Int Int
findAllPaths graph seen heap paths 
    | H.null heap = paths
    | otherwise = findAllPaths graph (next `S.insert` seen) (updateHeap w newCuts heap') (M.insert next w paths)
    where
    Just (next H.:-> w) = H.findMin heap
    heap' = H.deleteMin heap
    newCuts = filter (\(Edge s e w)-> not $ S.member e seen) (edges next graph) --This was missing from naive appraoch

    
    
{-
shortestPath' :: Int -> Int -> Graph -> M.Map Int Int -> M.Map Int Int
shortestPath' source target graph aStar = let
    initialV_X = minVertexWeights aStar $ edges source graph
    iHeap = foldl insert makeEmpty initialV_X :: LeftistHeap WeightedVertex
    in go iHeap aStar
    where
    go :: Heap h => h WeightedVertex -> M.Map Int Int -> M.Map Int Int
    go v_x aStar' = let
        m = getMin v_x 
        aStar'' = M.insert (vId (trace (show m) m)) (vWeight m) aStar'
        in case vId m == target of
            True -> aStar''
            False -> let
                v_x' = minVertexWeights aStar'' . concatMap (`edges` graph) $ M.keys aStar''
                iHeap' = foldl insert makeEmpty v_x' :: LeftistHeap WeightedVertex
                in go iHeap' aStar''
-}
updateHeap :: Int -> [Edge] -> PHeap -> PHeap
updateHeap wStar es heap = foldl f heap es
    where f acc (Edge _ v w) = H.adjust (\currValue -> min currValue $ wStar + w) v acc

{- This is my naive attempt at the O(mn) approach -}
minVertexWeights :: M.Map Int Int ->  [Edge] -> [WeightedVertex]
minVertexWeights aStar es = 
    fmap (\(v,w)-> WeightedVertex {vId= v,vWeight= w }) . M.toList $ foldl computeMin M.empty es
    where
    computeMin m (Edge s v w) = M.insertWith min v (fromMaybe 1000000 (s `M.lookup` aStar) + w) m

connectedComponent :: Int -> Graph -> Graph
connectedComponent source graph = let
    es = edges source graph
    initialSeen = S.singleton source
    iq = Q.makeEmpty :: LQueue Edge
    initialEdges = foldl (flip Q.push) iq es
    cc = go initialSeen initialEdges 
    in M.filterWithKey (\k _-> S.member k cc) graph
    where
    go :: Q.Queue q => S.Set Int -> q Edge -> S.Set Int
    go s q = let
        (q', e) = Q.pop q
        in case e of
            Nothing -> s
            Just (Edge _ e _) -> 
                if S.member e s
                then go s q'
                else go (S.insert e s) (foldl (flip Q.push) q' (edges e graph))
    
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


