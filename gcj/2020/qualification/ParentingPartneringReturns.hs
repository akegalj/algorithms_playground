{-# LANGUAGE TupleSections #-}
module ParentingPartneringReturns where

import Control.Monad
import Data.List
import Data.Ord
import Prelude

type Start = Int
type End = Int
type Index = Int
data Person
    = Cameron
    | Jamie
type CameronIsBusy = Maybe Task
type JamieIsBusy = Maybe Task

type Task = (Index, Start, End)

-- tasks sorted by index
newtype TasksByIndex = TasksByIndex [Task]

-- tasks sorted by start
newtype TasksByStart = TasksByStart [Task]

type SolvedTasks = [(Task, Person)]
newtype SolvedTasksByIndex = SolvedTasksByIndex SolvedTasks

type BusyPerson = (CameronIsBusy, JamieIsBusy)

type SimulationState = Maybe (SolvedTasks, BusyPerson)

type TestNum = Int
type TestCase = (TestNum, TasksByIndex)
type Output = (TestNum, Maybe SolvedTasksByIndex)

-- Task state machine
simulateTime :: TasksByStart -> Maybe SolvedTasksByIndex
simulateTime (TasksByStart ts) = fmap (sortSolvedTasksByIndex . fst) $ foldl nextTask initState ts
  where
    initState :: SimulationState
    initState = Just ([], (Nothing, Nothing))

    forwardTime :: Maybe Task -> Start -> Maybe Task
    forwardTime Nothing _ = Nothing
    forwardTime task@(Just (_,_,e)) currentTime
        | currentTime >= e = Nothing -- finish task
        | otherwise = task

    nextTask :: SimulationState -> Task -> SimulationState
    nextTask Nothing _ = Nothing
    nextTask (Just (solvedTasks, (camBusy, jamBusy))) task@(_,start,_) =
        let newBusy = (forwardTime camBusy start, forwardTime jamBusy start)
        in case newBusy of
            (Just _, Just _) -> Nothing                                             -- we don't have available time: IMPOSSIBLE
            (Nothing, jamie) -> Just ((task, Cameron):solvedTasks, (Just task, jamie))   -- assign task to cameron
            (cameron, Nothing) -> Just ((task, Jamie):solvedTasks, (cameron, Just task)) -- assign task to jamie

sortSolvedTasksByIndex :: SolvedTasks -> SolvedTasksByIndex
sortSolvedTasksByIndex = SolvedTasksByIndex . sortBy (comparing $ \((i,_,_),_) -> i)

sortTasksByStart :: TasksByIndex -> TasksByStart
sortTasksByStart (TasksByIndex ts) = TasksByStart $ sortBy (comparing $ \(_,s,_) -> s) ts

sortTasksByIndex :: TasksByStart -> TasksByIndex
sortTasksByIndex (TasksByStart ts) = TasksByIndex $ sortBy (comparing $ \(i,_,_) -> i) ts

readTasks :: IO TasksByIndex
readTasks = do
    n <- read <$> getLine :: IO Int
    fmap TasksByIndex . forM [1..n] $ \index -> do
        [start, end] <- map read . words <$> getLine
        pure (index, start, end)

computeOutput :: TestCase -> Output
computeOutput (tx, mx) =
    (tx, simulateTime $ sortTasksByStart mx)

outputTestCase :: Output -> String
outputTestCase (tc, mTasks) =
    "Case #" ++ show tc ++ ": " ++ showTasks
  where
    showTasks = case mTasks of
        Nothing -> "IMPOSSIBLE"
        Just (SolvedTasksByIndex tasks) -> map showPerson tasks
    showPerson (_, Cameron) = 'C'
    showPerson (_, Jamie) = 'J'

outputAll :: [Output] -> IO ()
outputAll = putStr . unlines . map outputTestCase

inputAll :: IO [TestCase]
inputAll = do
    t <- read <$> getLine :: IO Int
    forM [1..t] $ \tc ->
        (tc,) <$> readTasks

main :: IO ()
main = do
    input <- inputAll
    outputAll $ map computeOutput input
