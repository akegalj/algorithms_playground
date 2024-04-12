import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import Data.Char (ord)
import Data.List (foldl')

type Input = [String]

parseInput :: String -> Input
parseInput = splitOn ","

hash :: String -> Int
hash = foldl' (\h c -> (h + ord c)*17 `rem` 256) 0

part1 :: Input -> Int
part1 = sum . map hash

part2 :: Input -> Int
part2 = undefined

main = interact $ show . (part1 &&& part2) . parseInput