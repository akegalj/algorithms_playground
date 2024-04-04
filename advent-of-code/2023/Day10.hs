import Prelude hiding (lookup, filter)
import Data.Map (fromList, Map, lookup, keys, filter, insert, difference, size, findMax, intersection)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.List (maximumBy, (\\))
import Control.Arrow ((&&&))

type Coord = (Int, Int)
data Direction = North | East | South | West deriving (Eq, Show, Enum)
type PipeType = (Direction, Direction)
data Tile =
    Ground
  | Start
  | Pipe PipeType
  deriving (Eq, Show)

type Diagram = Map Coord Tile

opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite South = North
opposite West = East

contains :: Tile -> Direction -> Bool
contains (Pipe (x,y)) d = x == d || y == d
contains _ _ = False

direction :: Coord -> Coord -> Maybe Direction
direction (x1,y1) (x2,y2)
  | x1 == x2 && abs (y1 - y2) == 1 = Just $ if y1 > y2 then North else South
direction (x1,y1) (x2,y2)
  | y1 == y2 && abs (x1 - x2) == 1 = Just $ if x1 > x2 then West else East
direction _ _ = Nothing

isConnected :: Diagram -> Coord -> Coord -> Bool
isConnected d c1 c2 = case (direction c1 c2, lookup c1 d, lookup c2 d) of
  (Just dir, Just t1, Just t2) -> contains t1 dir && contains t2 (opposite dir)
  _ -> False

step :: Coord -> Direction -> Coord
step (x, y) d = case d of
  North -> (x, pred y)
  South -> (x, succ y)
  East  -> (succ x, y)
  West  -> (pred x, y)

findLoop :: Diagram -> Coord -> Maybe Diagram
findLoop d start = checkLoop . go (step start dir) $ opposite dir
  where
    Just (Pipe (dir, _)) = lookup start d
    go cur prevDir
      | cur == start = [start]
      | otherwise = cur : case lookup cur d of
                          Just (Pipe (dir1, dir2))
                            | dir1 /= prevDir && isConnected d cur (step cur dir1) -> go (step cur dir1) $ opposite dir1
                            | dir2 /= prevDir && isConnected d cur (step cur dir2) -> go (step cur dir2) $ opposite dir2
                          _ -> [] -- not a loop
    checkLoop xs
      | last xs == start = Just $ d `intersection` (fromList $ map (,Start) xs)
      | otherwise = Nothing

farthestLoop :: Diagram -> Diagram
farthestLoop d = maximumBy (comparing size) . catMaybes . map (\nd -> findLoop nd start) . map (\sPipe -> insert start sPipe d) $ map (Pipe . parsePipe) "|-JL7F"
  where
    start = head . keys $ filter (==Start) d

parsePipe :: Char -> PipeType
parsePipe '|' = (North, South)
parsePipe '-' = (East, West)
parsePipe 'J' = (North, West)
parsePipe 'L' = (North, East)
parsePipe '7' = (South, West)
parsePipe 'F' = (South, East)

parseTile :: Char -> Tile
parseTile '.' = Ground
parseTile 'S' = Start
parseTile p = Pipe $ parsePipe p

parseDiagram :: String -> Diagram
parseDiagram = fromList . concatMap (\(y, xs) -> map (\(x, t) -> ((x,y),parseTile t)) xs) . map (fmap $ zip [0..]) . zip [0..] . lines

countInside :: Diagram -> [[Coord]]
countInside d = map snd [ foldr (\x st -> count (x,y) st) (False, []) [0..maxX] | y <- [0..maxY]]
  where
    maxX = maximum . map fst $ keys d
    maxY = maximum . map snd $ keys d
    count c (f, xs) = case lookup c d of
      Nothing -> (f, if f then c : xs else xs)
      Just Start -> error "boom start"
      Just Ground -> error "boom ground"
      Just pipe
        | pipe `elem` [Pipe (East, West), Pipe (South, East), Pipe (South, West)] -> (f, xs)
        | otherwise -> (not f, xs)

part1 :: Diagram -> Int
part1 = (`div` 2) . size . farthestLoop

part2 :: Diagram -> Int
part2 = length . concat . countInside . farthestLoop

main = interact $ show . (part1 &&& part2) . parseDiagram