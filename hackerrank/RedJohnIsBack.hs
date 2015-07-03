{-# LANGUAGE BangPatterns #-}
-- https://www.hackerrank.com/challenges/red-john-is-back

import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.List (foldl')

wall :: Int -> Int
wall n = if n < 4
            then 1
            else wall (n-1) + wall (n-4)

prims :: [Int]
prims = reverse . fst $ foldl' markPrim ([],S.replicate (maxWall+1) False) [2..maxWall]
  where maxWall = last wall'
        markPrim (!p,!xs) !n | S.index xs n == False = (n:p, update xs n)
                             | otherwise = (p,xs)
        update xs n = foldl' (\ac k -> S.update k True ac) xs [n*2,n*3..maxWall]

primsWall :: Int -> Int
primsWall k = (!!(k-1)) $ map (length . flip takeWhile prims . flip (<=)) wall'

-- HARDCODED SOLUTION BELOW

wall' :: [Int]
wall' = [1,1,1,2,3,4,5,7,10,14,19,26,36,50,69,95,131,181,250,345,476,657,907,1252,1728,2385,3292,4544,6272,8657,11949,16493,22765,31422,43371,59864,82629,114051,157422,217286]

primsWall' :: Int -> Int
primsWall' k = [0,0,0,1,2,2,3,4,4,6,8,9,11,15,19,24,32,42,53,68,91,119,155,204,269,354,462,615,816,1077,1432,1912,2543,3385,4522,6048,8078,10794,14475,19385]!!(k-1)

main = do
  input <- getContents
  mapM_ (print . primsWall' . read) . tail $ lines input
