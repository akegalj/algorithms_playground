import Control.Arrow ((&&&))
import Data.List (foldl')

type Coord = (Int, Int)
data Direction = U | D | L | R deriving (Show, Read, Eq)
type DigPlan = [((Direction, Int), (Direction, Int))]

parseInput :: String -> DigPlan
parseInput = map (parseItem . words) . lines
  where
    parseItem [d,l,c] =
      let (hex,dr) = splitAt 5 . init $ drop 2 c
      in ((read d, read l), ([R,D,L,U] !! read dr, read $ "0x"<>hex))

area :: [Coord] -> Int
area all@(x:xs) = (`div` 2) . sum $ zipWith det all (xs++[x])
  where
    det (x1,y1) (x2,y2) = x1*y2 - y1*x2

coords :: [(Direction,Int)] -> [Coord]
coords = init . foldl' addPoint [(0,0)]
  where
    addPoint all@((x,y):_) (d,l) = case d of
      U -> (x,y+l):all
      D -> (x,y-l):all
      R -> (x+l,y):all
      L -> (x-l,y):all

part1 :: DigPlan -> Int
part1 dp' = area cs + countRD + 1
  where
    dp = map fst dp'
    cs = coords dp
    countRD = sum . map snd $ filter ((`elem` [R,D]) . fst) dp

part2 :: DigPlan -> Int
part2 = part1 . map (\(a,b) -> (b,b))

main = interact $ show . (part1 &&& part2) . parseInput
