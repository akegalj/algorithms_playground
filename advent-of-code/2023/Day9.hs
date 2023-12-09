import Control.Arrow ((&&&))
import Data.List

part1, part2 :: [[Int]] -> Int
part1 = sum . map (foldl1' (+) . map last . extrapolate)
part2 = sum . map (foldr1 (-) . map head . extrapolate)

extrapolate :: [Int] -> [[Int]]
extrapolate [x] = [[x]]
extrapolate all@(_:xs) = all : extrapolate (zipWith (-) xs all)

parse :: String -> [[Int]]
parse = map (map read . words) . lines

main = interact $ show . (part1 &&& part2) . parse