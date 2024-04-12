{-# LANGUAGE TypeFamilies #-}
import Control.Arrow ((&&&))
import Data.List (transpose, foldl', groupBy, sort, iterate')
import Debug.Trace
import Prelude hiding (cycle)
import Data.MemoTrie
import GHC.Generics (Generic)

data Tile = Round | Cube | Space deriving (Eq, Ord, Show, Generic)
type Dish = [[Tile]]

showDish :: Dish -> String
showDish = unlines . map (map showTile)
  where
    showTile Round = 'O'
    showTile Cube = '#'
    showTile Space = '.'

instance HasTrie Tile where
  newtype (Tile :->: b) = TileTrie { unTileTrie :: Reg Tile :->: b }
  trie = trieGeneric TileTrie
  untrie = untrieGeneric unTileTrie
  enumerate = enumerateGeneric unTileTrie

parseInput :: String -> Dish
parseInput = map (map parseRock) . lines
  where
    parseRock 'O' = Round
    parseRock '#' = Cube
    parseRock '.' = Space

loadLeft :: Dish -> Int
loadLeft = sum . map (snd . foldl' count (0,0) . zip [0..] . reverse . (Cube:))
  where
    count (c,s) (i,t)
      | t == Round = (c+1,s)
      | t == Cube = (0, s + i*c - c*(c-1) `div` 2)
      | otherwise = (c,s)

cycle :: Dish -> Dish
cycle = memo $ pullRight . pullDown . pullLeft . pullUp
  where
    pullLeft = map $ concatMap sort . groupBy (\a b -> a /= Cube && b /= Cube)
    pullUp = transpose . pullLeft . transpose
    pullRight = map reverse . pullLeft . map reverse
    pullDown = reverse . pullUp . reverse -- transpose . pullRight . transpose

part1 :: Dish -> Int
part1 = loadLeft . transpose

part2 :: Dish -> Int
part2 = sum . map part1 . take (10^9) . iterate' cycle

main = interact $ show . (part1 &&& part2) . parseInput