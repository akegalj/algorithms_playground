import Control.Arrow ((&&&))
import Data.List

part1, part2 :: [[Int]] -> Int
part1 = sum . map (sum . map last . extrapolate)
part2 = part1 . map reverse

extrapolate :: [Int] -> [[Int]]
extrapolate = takeWhile (any (/=0)) . iterate (zipWith subtract <*> tail)

parse :: String -> [[Int]]
parse = map (map read . words) . lines

main = interact $ show . (part1 &&& part2) . parse