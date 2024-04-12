import Control.Arrow ((&&&))
import Data.List (transpose, foldl')
import Debug.Trace

data Tile = Round | Cube | Space deriving (Eq, Show)
type Dish = [[Tile]]

parseInput :: String -> Dish
parseInput = map (map parseRock) . lines
  where
    parseRock 'O' = Round
    parseRock '#' = Cube
    parseRock '.' = Space

part1 :: Dish -> Int
part1 = sum . map (snd . foldl' count (0,0) . zip [0..] . reverse . (Cube:)) . transpose
  where
    count (c,s) (i,t)
      | t == Round = (c+1,s)
      | t == Cube = (0, s + i*c - c*(c-1) `div` 2)
      | otherwise = (c,s)

part2 :: Dish -> Int
part2 = const 0

main = interact $ show . (part1 &&& part2) . parseInput