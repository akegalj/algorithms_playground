-- https://www.hackerrank.com/contests/101hack23/challenges/devu-magical-girl-and-spirits
{-# LANGUAGE BangPatterns #-}
import Data.List (foldl')

bridge :: [Int] -> Bool -> Int -> Int -> Int
bridge [] _ _ _ = 0
bridge (x:xs) False l m = if l + x < 0
                             then 0
                             else 1 + bridge xs False (l+x) m
bridge (x:xs) True l m  = if l + x < 0
                             then 1 + bridge xs False (l + x - 2 * min m x) m
                             else 1 + bridge xs True (l+x) (min m x)

solve :: [Int] -> String
solve xs = didSheMakeIt $ bridge xs True 0 0
  where didSheMakeIt l = if length xs == l then "She did it!" else show $ l + 1

main = do
  input <- getContents
  mapM_ (putStrLn . solve . map read . words . snd) . filter (even . fst) . zip [1..] . tail $ lines input

