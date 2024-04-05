import Control.Arrow ((&&&))
import qualified Data.Set as S
import Data.Function (on)
import Data.List (groupBy)

type Image = S.Set Galaxy
type Galaxy = (Int, Int)

parseInput :: String -> Image
parseInput = S.fromList . map fst . filter ((=='#') . snd) . index
  where
    index = concatMap (\(y,xs) -> map (\(x,c) -> ((x,y),c)) xs) . map (fmap $ zip [0..]) . zip [0..] . lines

expandGalaxy :: Image -> Image
expandGalaxy i = S.map (\(x,y) -> (x + count (<x) emptyCol, y + count (<y) emptyRow)) i
  where
    maxX = S.findMax $ S.map fst i
    maxY = S.findMax $ S.map snd i
    emptyCol = S.fromList [0..maxX] S.\\ S.map fst i
    emptyRow = S.fromList [0..maxY] S.\\ S.map snd i
    count f = length . S.takeWhileAntitone f

distance :: Galaxy -> Galaxy -> Int
distance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

part1 :: Image -> Int
part1 i = (`div` 2) . sum . map (uncurry distance) . S.toList $ S.cartesianProduct i i

part2 :: Image -> Int
part2 = const 0

main = interact $ show . (part1 &&& part2) . expandGalaxy . parseInput