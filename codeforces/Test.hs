import Data.List
import Control.Arrow ((&&&))

parseInput :: String -> [[String]]
parseInput = map (map snd) . groupBy (\(a,_) (b,_) -> odd a && even b) . zip [1..] . tail . lines

solve :: [String] -> [String]
solve [a,b] = (\(a,b) -> [a,b]) . unzip $ calc left ++ swap (calc right)
  where
    r = length a `div` 2
    l = length a - r
    (left, right) = splitAt l $ zip a b
    swap = map $ uncurry $ flip (,)
    calc = map (uncurry max &&& uncurry min)

main = interact $ unlines . concatMap solve . parseInput