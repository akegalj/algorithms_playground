import Control.Arrow ((&&&))
import Data.List

part1, part2 :: [[Int]] -> Int
part1 = sum . map extrapolate
part2 = const 0

extrapolate :: [Int] -> Int
extrapolate = foldl1' (+) . map last . go
  where
    go [x] = [[x]]
    go all@(_:xs) = all : go (zipWith (-) xs all)

parse :: String -> [[Int]]
parse = map (map read . words) . lines

main = interact $ show . (part1 &&& part2) . parse