{-# LANGUAGE TupleSections #-}
module Pylons where

import Control.Monad
import Data.List
import Prelude
import Safe

type RowNum = Int
type ColNum = Int
type GalaxyDimension = (RowNum, ColNum)
type Coords = GalaxyDimension

---------------------
type Result = Maybe [Coords]
type Data = GalaxyDimension
---------------------

invalidCoords :: GalaxyDimension -> [Coords] -> [Coords]
invalidCoords = undefined

allCoords :: GalaxyDimension -> [Coords]
allCoords (row, col) = [(r,c) | r <- [1..row], c <- [1..col]]

validJumps :: GalaxyDimension -> [Coords] -> [Coords]
validJumps gm js = allCoords gm \\ invalidCoords gm js

-- Pick any valid coord
nextCoords :: GalaxyDimension -> [Coords] -> Maybe Coords
nextCoords gm = headMay . validJumps gm

-- Algorithmic solution should go here
computeOutput :: GalaxyDimension -> Maybe [Coords]
computeOutput (row, col) = undefined

-- Convert result to string that will be printed out
testCaseResult :: Maybe [Coords] -> String
testCaseResult Nothing = "IMPOSSIBLE"
testCaseResult (Just coords) = "POSSIBLE\n" ++ outputCoords
  where
    outputCoords = unlines $ map outputCoord coords
    outputCoord (r,c) = show r ++ " " ++ show c

-- Read data from stdin
readGalaxyDimension :: IO GalaxyDimension
readGalaxyDimension = do
    [row, col] <- map read . words <$> getLine
    pure (row, col)

readTestCase :: IO Data
readTestCase = readGalaxyDimension

-- Test case handling

type TestNum = Int
type TestCase = (TestNum, Data)
type Output = (TestNum, Result)

-- Output to stdout

outputTestCase :: Output -> String
outputTestCase (tc, result) =
    "Case #" ++ show tc ++ ": " ++ testCaseResult result

outputAll :: [Output] -> IO ()
outputAll = putStr . unlines . map outputTestCase

-- Output from stdin

inputAll :: IO [TestCase]
inputAll = do
    t <- read <$> getLine :: IO Int
    forM [1..t] $ \tc ->
        (tc,) <$> readTestCase

main :: IO ()
main = do
    input <- inputAll
    outputAll $ map (fmap computeOutput) input
