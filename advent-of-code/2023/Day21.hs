import Control.Arrow ((&&&))
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.List (find)

type Coord = (Int, Int)
type Start = Coord
type Plots = S.Set Coord

parseInput :: String -> (Plots, Start)
parseInput = (plots &&& start) . concatMap (\(y,l) -> map (\(x,c) -> ((x,y),c)) l) . map (fmap $ zip [0..]) . zip [0..] . lines
  where
    plots = S.fromList . map fst . filter ((/= '#') . snd)
    start = fst . fromJust . find ((== 'S') . snd)

neighbourPlots :: Start -> Plots -> Plots
neighbourPlots (x,y) = S.intersection neighbours
  where
    neighbours = S.fromList [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

-- TODO: check the solution. Somehow it is a wrong answer although it looks good
part1 :: (Plots, Start) -> Int
part1 (pl, st) = getResult . iterate nextStep $ S.singleton st
  where
    nextStep = S.unions . S.map (flip neighbourPlots pl)
    getResult = length . (!! 64)

part2 :: (Plots, Start) -> Int
part2 = const 0

main = interact $ show . (part1 &&& part2) . parseInput