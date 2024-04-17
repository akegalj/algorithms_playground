import Control.Arrow ((&&&))
import Data.List (foldl')
import qualified Data.Set as S
import Data.Bifunctor (first, second)

type Coord = (Int, Int)
data Direction = U | D | L | R deriving (Show, Read, Enum)
type RGB = Int -- Int24 or (Word8, Word8, Word8)
type DigPlan = [(Direction, Int, RGB)]
type DigPath = S.Set Coord

parseInput :: String -> DigPlan
parseInput = map (parseItem . words) . lines
  where
    parseItem [d,l,c] = (read d, read l, read . ("0x"++) . drop 2 $ init c)

area :: [Coord] -> Int
area all@(x:xs) = (`div` 2) . sum $ zipWith det all (xs++[x])
  where
    det (x1,y1) (x2,y2) = x1*y2 - y1*x2

move :: Direction -> Coord -> Coord
move U = second succ
move D = second pred
move L = first pred
move R = first succ

coords :: DigPlan -> [Coord]
coords = init . foldl' addPoint [(0,0)]
  where
    addPoint all@((x,y):_) (d,l,_) = case d of
      U -> (x-1,y+l):all
      D -> (x,y-l):all
      R -> (x+l,y-1):all
      L -> (x-l,y):all

dig :: DigPlan -> DigPath
dig = S.fromList . foldl' addLine [(0,0)]
  where
    addLine all (_,0,_) = all
    addLine all@(c:_) (d,l,h) = addLine (move d c : all) (d,pred l,h)

showPath :: DigPath -> String
showPath dp = unlines . reverse $ map (\y -> [ if (x,y) `S.member` dp then '#' else '.' | x <- [minX..maxX]]) [minY..maxY]
  where
    minX = S.findMin $ S.map fst dp
    maxX = S.findMax $ S.map fst dp
    minY = S.findMin $ S.map snd dp
    maxY = S.findMax $ S.map snd dp

interior :: DigPath -> DigPath
interior dp = all S.\\ (dp <> exterior)
  where
    all = S.fromList [ (x,y) | x <- [minX..maxX], y <- [minY..maxY] ]
    extStart = S.fromList [ (x,y) | x <- [minX..maxX], y <- [minY..maxY], x == minX || x == maxX || y == minY || y == maxY ] S.\\ dp
    exterior = S.foldl' fill mempty extStart
    fill ex c
      | c `S.member` ex || c `S.member` dp || c `S.notMember` all= ex
      | otherwise = foldl' fill (S.insert c ex) $ map (flip move c) [U .. R]
    minX = S.findMin $ S.map fst dp
    maxX = S.findMax $ S.map fst dp
    minY = S.findMin $ S.map snd dp
    maxY = S.findMax $ S.map snd dp

part1 :: DigPlan -> Int
part1 = S.size . ((<>) <*> interior) . dig

part2 :: DigPlan -> Int
part2 = const 0

main = interact $ show . (part1 &&& part2) . parseInput
