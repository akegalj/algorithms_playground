-- https://www.hackerrank.com/challenges/sherlock-and-anagrams
import Data.List (groupBy, sortBy, sort)
import Data.Function (on)

isAnagram :: String -> String -> Bool
isAnagram a b = sort a == sort b

solve :: String -> Int
solve = countAnagrams . anagramPairs . anagrams
  where anagrams xs = [ take t $ drop d xs | d <- [0..length xs - 1], t <- [1..length xs - d] ]
        countAnagrams = sum . map (length . filter (uncurry isAnagram))
        anagramPairs = let createPairs xs = [ (x,y) | (xi,x) <- zip [1..] (init xs), y <- drop xi xs ]
                       in map createPairs . groupBy ((==) `on` length) . sortBy (compare `on` length)

main = do
  input <- getContents
  mapM_ (print . solve) . tail $ lines input
