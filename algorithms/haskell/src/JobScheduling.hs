{-# LANGUAGE RecordWildCards #-}

module JobScheduling (
    byDiff,
    byRatio
) where

import Data.List (sortBy, groupBy, concat)
import Data.Ord (comparing)
import Text.Parsec
import Text.Parsec.Char (endOfLine)
import Text.Parsec.String

import Debug.Trace

data Job = Job {weight :: Integer, len :: Integer}
    deriving (Eq, Show)

byRatio ::
    String ->
    IO Integer
byRatio = fmap (weightedCompletionTime . sortByRatio) . jobList

byDiff ::
    String ->
    IO Integer
byDiff = fmap (weightedCompletionTime . sortByDiff) . jobList

sortByDiff ::
    [Job] ->
    [Job]
sortByDiff = concatMap groupSort . groupBy groupKey . sortBy (flip $ comparing jobDiff)
    -- ^ This is reasonably slow because I'm using lists (and reverse, sort, group, etc...)
    where
    jobDiff Job {..} = weight - len
    groupSort :: [Job] -> [Job]
    groupSort = sortBy (flip $ comparing weight)
    groupKey l r = jobDiff l == jobDiff r

sortByRatio ::
    [Job] ->
    [Job]
sortByRatio = sortBy (flip $ comparing jobRatio)
    where
    jobRatio (Job {..}) = fromInteger weight / fromInteger len

weightedCompletionTime ::
    [Job] ->
    Integer
weightedCompletionTime = sum . fmap weighted . compTimes
    where
    weighted Job {..} = weight * len

compTimes (p:j:js) = let
    newJ = j {len= len j + len p }
    in p:compTimes (newJ:js)
compTimes [j] = [j]

jobList :: FilePath -> IO [Job]
jobList f = do
    raw <- readFile  f
    case parse parseLines "jobsParser" raw of
        Left e -> error (show e)
        Right jobs -> pure jobs

parseLines :: Parser [Job]
parseLines = parseLine `sepEndBy` endOfLine

parseLine :: Parser Job
parseLine = do
    w <- n
    char ' '
    l <- n
    pure Job {weight = w, len = l}
    where n = read <$> many1 digit
