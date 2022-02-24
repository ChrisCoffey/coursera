module MaxKTest where
-- where the target number k of clusters is set to 4. 
-- What is the maximum spacing of a 4-clustering?

import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Monad.ST
import qualified Data.Heap as H
import System.Environment
import Data.Array.MArray
import Data.Array.ST
import Data.STRef

type Start = Int
type End = Int
type Dist = Int   -- dist must be symmetric

--EdgesHeap: Edges, ordered by Dist and containing start and end
type EdgesHeap = H.MinPrioHeap Dist (Start, End)

run args = do
	--iHeap <- getGraph "q1test.txt"
	iHeap <- getGraph $ head args
	let 
		len = 500
		res = runST $ do
			uf <- newUnionFind (len + 1)
			findNext uf (len - 4) iHeap

	return $ H.viewHead res
	--return res

findNext :: (UnionFind s) -> Int -> EdgesHeap -> ST s EdgesHeap
findNext uf c hp = do
	let Just ((_,(s,f)), hp') = H.view hp
	test <- find uf s f
	if test then			-- true if top of heap in same cluster
		findNext uf c hp'
	else if c > 0 then do
		unite uf s f
		findNext uf (c-1) hp'
		else return hp

getGraph :: String -> IO EdgesHeap
getGraph path = do
    lines <- (map BS.words . BS.lines) `fmap` BS.readFile path
    let triples = (map . map) (maybe (error "can't read Int") fst . BS.readInt) lines
    return $ H.fromList [(d, (e,s)) | [s, e, d] <- triples]
 

-- ####################################################################################
-- Code from http://kwangyulseo.com/2014/01/30/implementing-union-find-algorithms-in-haskell/
data UnionFind s = UnionFind {
    ids:: STUArray s Int Int
  , szs:: STUArray s Int Int
  }
 
newUnionFind :: Int -> ST s (UnionFind s)
newUnionFind n = liftM2 UnionFind (newListArray (0, n-1) [0..n-1]) (newArray (0, n-1) 1)
 
find :: (UnionFind s) -> Int -> Int -> ST s Bool
find uf p q = liftM2 (==) (root uf p) (root uf q)

root :: (UnionFind s) -> Int -> ST s Int
root uf i = do
    id <- readArray (ids uf) i
    if (id /= i)
        then do
            gpid <- readArray (ids uf) id
            writeArray (ids uf) i gpid
            root uf id
        else return i
 
unite :: (UnionFind s) -> Int -> Int -> ST s ()
unite uf p q = do
    i <- root uf p
    j <- root uf q
    szi <- readArray (szs uf) i
    szj <- readArray (szs uf) j
    if (szi < szj)
        then do
            writeArray (ids uf) i j
            writeArray (szs uf) j (szi + szj)
        else do
            writeArray (ids uf) j i
            writeArray (szs uf) i (szj + szi)

-- start with each point in individual clusters
-- loop
	 --find next cut (heap can contain internal links)
	 --find min spacing between clusters
	 --merge these two clusters
		-- remove all internal links?
-- stop when have k-clusters (= n-k rounds)
