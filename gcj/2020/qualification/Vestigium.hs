{-# LANGUAGE TupleSections #-}
module Vestigium where

import Control.Monad
import Data.List
import Prelude

type Matrix = [[Int]]
type TestNum = Int
type Trace = Int
type RepeatedRowNum = Int
type RepeatedColNum = Int
type TestCase = (TestNum, Matrix)
type Output = (TestNum, Trace, RepeatedRowNum, RepeatedColNum)

repeatedRowNum :: Matrix -> RepeatedRowNum
repeatedRowNum mx@(mxHead:_) =
    length . filter (/= size) . map length $ map nub mx
  where
    size = length mxHead

repeatedColNum :: Matrix -> RepeatedColNum
repeatedColNum = repeatedRowNum . transponseMatrix

matrixTrace :: Matrix -> Trace
matrixTrace mx = sum . map (uncurry (!!)) $ zip mx [0..]

-- This will compute reversed transponse matrix
-- It is more efficient then non-reversed as we can
-- use lists cons operator and append to previous lists
-- head
transponseMatrix :: Matrix -> Matrix
transponseMatrix (x:xs) =
    foldl (\old new -> map (uncurry (:)) $ zip new old) (map (:[]) x) xs

readMatrix :: IO Matrix
readMatrix = do
    n <- read <$> getLine :: IO Int
    forM [1..n] $ \_ ->
        map read . words <$> getLine

computeOutput :: TestCase -> Output
computeOutput (tx, mx) =
    (tx, matrixTrace mx, repeatedRowNum mx, repeatedColNum mx)

outputTestCase :: Output -> String
outputTestCase (tc, tr, rn, cn) =
    "Case #" ++ show tc ++ ": " ++ unwords (map show [tr, rn, cn])

outputAll :: [Output] -> IO ()
outputAll = putStr . unlines . map outputTestCase

inputAll :: IO [TestCase]
inputAll = do
    t <- read <$> getLine :: IO Int
    forM [1..t] $ \tc ->
        (tc,) <$> readMatrix

main :: IO ()
main = do
    input <- inputAll
    outputAll $ map computeOutput input
