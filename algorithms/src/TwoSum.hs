module TwoSum where

import Data.Array.BitArray.IO

import qualified Data.Set as S

import Control.Monad (when)
import Data.List (nub, sort, foldl')
import Debug.Trace

targets :: [Int]
targets = [-10000..10000]

buildSet :: [Int] -> IO (IOBitArray Int)
buildSet ns = do
    arr <- newArray (0, 104395301) False
    mapM_ (g arr) ns
    pure arr
    where 
    g :: IOBitArray Int -> Int -> IO ()
    g bs x = writeArray bs (hashFunc x) True

buildSet' :: [Int] -> S.Set Int
buildSet' ns = foldl' (\s x-> x `S.insert` s) S.empty ns

targetMatches' :: [Int] -> S.Set Int -> Int
targetMatches' ns s = S.size $ foldl' (f targets) S.empty ns
    where
    f :: [Int] -> S.Set Int -> Int -> S.Set Int
    f [] seen _ = seen
    f (t:rest) seen x = 
        if (t-x) `S.member` s
        then f rest (t `S.insert` seen) x -- (trace (show (t-x) ++ " : " ++ (show x) ++ " : " ++ show t) x)
        else f rest seen x

doTwoSum' :: [Int] -> Int
doTwoSum' ns = let
    nss = sort . S.toList $ S.fromList ns
    s = buildSet' nss
    in targetMatches' nss s

doTwoSum :: [Int] -> IO Int
doTwoSum ns = do
    let nss = S.toList . S.fromList $ sort ns
    bits <- buildSet nss
    targetMatches nss bits 

targetMatches :: [Int] -> IOBitArray Int -> IO Int
targetMatches ns s = do
    seen <- newArray (-10000, 10000) False
    mapM_ (f targets seen) ns
    popCount seen
    where
    f :: [Int] -> IOBitArray Int -> Int -> IO ()
    f [] seen x = pure ()
    f (t:rest) seen x = do
        isMember <- s `readArray` (hashFunc (t-x))
        if isMember 
        then writeArray seen t True
        else f rest seen x

hashFunc :: Int -> Int
hashFunc n = (n * 2654435761) `mod` 104395301

--Mostly a failed experiment
{-
-class Bits a => BitSet a where
-    member :: Int -> a -> Bool
-    insert :: Int -> a -> a
-    empty :: a
-
-instance BitSet Integer where
-    empty = 0
-    insert b s = setBit s (hashFunc b)
-    member b s = testBit s (hashFunc b)
-}
