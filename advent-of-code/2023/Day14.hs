import Control.Arrow ((&&&))
import Data.List (transpose, foldl', groupBy, sort, iterate')
import Prelude hiding (cycle)
import qualified Data.Set as S

data Tile = Round | Cube | Space deriving (Eq, Ord, Show)
type Dish = [[Tile]]

parseInput :: String -> Dish
parseInput = map (map parseRock) . lines
  where
    parseRock 'O' = Round
    parseRock '#' = Cube
    parseRock '.' = Space

load :: Dish -> Int
load = sum . map (\(i,l) -> (*i) . length $ filter (==Round) l) . zip [1..] . reverse

cycle :: Dish -> Dish
cycle = pullRight . pullDown . pullLeft . pullUp

pullLeft = map $ concatMap sort . groupBy (\a b -> a /= Cube && b /= Cube)
pullUp = transpose . pullLeft . transpose
pullRight = map reverse . pullLeft . map reverse
pullDown = reverse . pullUp . reverse -- transpose . pullRight . transpose

part1 :: Dish -> Int
part1 = load . pullUp

part2 :: Dish -> Int
part2 = load . index 1000000000 . findCycle mempty mempty . iterate' cycle
  where
    index i (ls,cs) = cs !! ((i - length ls) `rem` length cs)
    findCycle s res (x:xs)
      | x `S.member` s = let rres = reverse res in (takeWhile (/=x) rres, dropWhile (/=x) rres)
      | otherwise = findCycle (S.insert x s) (x:res) xs

main = interact $ show . (part1 &&& part2) . parseInput