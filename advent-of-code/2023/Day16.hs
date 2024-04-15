import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.Bifunctor (first, second)
import qualified Data.Set as S
import Debug.Trace

data Tile = Space | MirrorNE | MirrorSE | SplitterNS | SplitterEW deriving (Show, Eq)
data Direction = N | E | W | S deriving (Show, Eq, Ord)
type Coord = (Int, Int)
type Puzzle = M.Map Coord Tile

move :: Direction -> (Coord, Direction) -> (Coord, Direction)
move N = (,N) . second pred . fst
move S = (,S) . second succ . fst
move E = (,E) . first succ . fst
move W = (,W) . first pred . fst

parseInput :: String -> Puzzle
parseInput = M.fromList . concatMap (\(y,l) -> map (\(x,c) -> ((x,y), parseTile c)) l) . map (fmap $ zip [0..]) . zip [0..] . lines
  where
    parseTile '.' = Space
    parseTile '/' = MirrorNE
    parseTile '\\' = MirrorSE
    parseTile '|' = SplitterNS
    parseTile '-' = SplitterEW

energize :: (Coord, Direction) -> Puzzle -> S.Set (Coord,Direction) -> S.Set (Coord,Direction)
energize x@(coord,dir) p visited
  | x `S.member` visited = visited
  | otherwise = case (M.lookup coord p, dir) of
      (Nothing,_) -> visited
      (Just Space,_) -> go dir
      (Just MirrorNE,E) -> go N
      (Just MirrorNE,S) -> go W
      (Just MirrorNE,W) -> go S
      (Just MirrorNE,N) -> go E
      (Just MirrorSE,E) -> go S
      (Just MirrorSE,S) -> go E
      (Just MirrorSE,W) -> go N
      (Just MirrorSE,N) -> go W
      (Just SplitterNS,_)
        | dir == E || dir == W -> energize (move S x) p $ go N
        | otherwise -> go dir
      (Just SplitterEW,_)
        | dir == N || dir == S -> energize (move W x) p $ go E
        | otherwise -> go dir
  where
    go d = energize (move d x) p $ S.insert x visited

showEnergized :: Puzzle -> S.Set (Coord, Direction) -> String
showEnergized p s = unlines $ map (\y -> [ if (x,y) `S.member` visited then '#' else '.' | x <- [0..maxX]]) [0..maxY]
  where
    ((maxX, maxY),_) = M.findMax p
    visited = S.map fst s

part1 :: Puzzle -> Int
part1 p = S.size . S.map fst $ energize ((0,0),E) p mempty

part2 :: Puzzle -> Int
part2 p = maximum $ map (\start -> S.size . S.map fst $ energize start p mempty) start
  where
    ((maxX, maxY),_) = M.findMax p
    start = concat [up, down, left, right]
    up = map (\x -> ((x,0),S)) [0..maxX]
    down = map (\x -> ((x,maxY),N)) [0..maxX]
    left = map (\y -> ((0,y),E)) [0..maxY]
    right = map (\y -> ((maxX,y),W)) [0..maxY]

main = interact $ show . (part1 &&& part2) . parseInput