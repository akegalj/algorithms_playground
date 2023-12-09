import Control.Arrow ((&&&))
import Data.List

part1, part2 :: [[Int]] -> Int
part1 = sum . map (foldl1' (+) . map last . extrapolate)
part2 = sum . map (foldl1' (flip (-)) . reverse . map head . extrapolate)

extrapolate :: [Int] -> [[Int]]
extrapolate = go
  where
    go [x] = [[x]]
    go all@(_:xs) = all : go (zipWith (-) xs all)

parse :: String -> [[Int]]
parse = map (map read . words) . lines

main = interact $ show . (part1 &&& part2) . parse