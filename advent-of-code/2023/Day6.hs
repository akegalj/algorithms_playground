import Control.Arrow ((&&&))
import Data.Bifunctor

type Input = [(Int, Int)]

part1 :: Input -> Int
part1 = product . map (length . solve)

part2 :: Input -> Int
part2 = length . solve . fixInput
  where
    fixInput = bimap fix fix . unzip
    fix = read . concatMap show

solve :: (Int, Int) -> [Int]
solve (t, d) = filter (>d) $ map (\s -> (t - s)*s) [1..t]

parse :: String -> Input
parse = (\[a,b] -> zip a b) . map (map read . tail . words) . lines

main = interact $ show . (part1 &&& part2) . parse