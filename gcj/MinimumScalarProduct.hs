{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Data.List (sortBy, sort)
import Data.List.Split (chunksOf)

data TestCase = Case CaseNum (Vector, Vector)
data Solution = Solution CaseNum MinScalarProduct
type MinScalarProduct = Int
type CaseNum = Int
type Vector = [Int]

instance Show Solution where
  show (Solution cn msp) = "Case #" ++ show cn ++ ": " ++ show msp

readTests :: String -> [TestCase]
readTests = map createTestCase . zip [1..] . chunksOf 3 . tail . lines
  where createTestCase :: (CaseNum, [String]) -> TestCase
        createTestCase (cn, _:v1:v2:[]) = Case cn (string2vector v1, string2vector v2)
        string2vector :: String -> Vector
        string2vector = map read . words

-- | Uses mergesort but could be more efficient to use bucket/radix sort. FIXME
solve :: TestCase -> Solution
solve (Case cn (v1,v2)) = Solution cn $ scalarProduct (sort v1) (sortBy (flip compare) v2)
  where scalarProduct :: Vector -> Vector -> MinScalarProduct
        scalarProduct v1 v2 = sum $ zipWith (*) v1 v2

main = do
  input <- getArgs >>= readFile . head
  mapM_ print . map solve $ readTests input
