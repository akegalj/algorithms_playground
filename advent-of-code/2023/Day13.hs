import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (transpose)

data Field = Dot | Hash deriving (Eq, Show)
type Coord = (Int, Int)
type Pattern = [[Field]]

parseInput :: String -> [Pattern]
parseInput = map parsePattern . splitOn "\n\n"
  where
    parsePattern = map (map parseField) . lines
    parseField '.' = Dot
    parseField '#' = Hash

horizontal :: Pattern -> [Int]
horizontal xs = map fst . filter snd . zip [1..] . zipWith (==) xs $ tail xs

vertical :: Pattern -> [Int]
vertical = horizontal . transpose

part1 :: [Pattern] -> Int
part1 = sum . map (\p -> sum (horizontal p) * 100 + sum (vertical p))

part2 :: [Pattern] -> Int
part2 = const 0

main = interact $ show . (part1 &&& part2) . parseInput
