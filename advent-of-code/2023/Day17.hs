import Control.Arrow ((&&&))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor (first, second)
import Data.List (sortOn)

data Direction = N | E | S | W deriving (Show, Eq, Enum, Ord)
type Coord = (Int, Int)
type Steps = Int
type Puzzle = M.Map Coord Int
type CDS = ((Coord,Direction),Steps)
type ICDS = (Int,(Coord,Direction),Steps)

parseInput :: String -> Puzzle
parseInput = M.fromList . concatMap (\(y,l) -> map (\(x,c) ->((x,y), read [c])) l) . map (fmap $ zip [0..]) . zip [0..] . lines

rotateLeft, rotateRight :: Direction -> Direction
rotateLeft  d = (!!3) $ [d..W] <> [N .. W]
rotateRight d = (!!1) $ [d..W] <> [N .. W]

move :: Direction -> (Coord, Direction) -> (Coord, Direction)
move N = (,N) . second pred . fst
move S = (,S) . second succ . fst
move E = (,E) . first succ . fst
move W = (,W) . first pred . fst

bsp :: Puzzle -> S.Set CDS -> (ICDS, S.Set ICDS) -> Int
bsp pm seen ((res,cd@(c,d),s),xs)
  | c == target = res
  | otherwise = bsp pm newSeen . S.deleteFindMin $ toVisit <> xs
  where
    (target,_) = M.findMax pm
    fst3 (x,_,_) = x
    snd3 (_,x,_) = x
    thr3 (_,_,x) = x
    fmove = flip move
    addRes (res,(c,d),s) = (res + pm M.! c,(c,d),s)
    validSteps = filter ((<4) . thr3)
    validCoord = filter (\(_,cd@(c,_),s) -> (cd,s) `S.notMember` seen && c `M.member` pm)
    toVisit = S.fromList . map addRes . validCoord . validSteps $
      [ (res,(fmove cd d),succ s)
      , (res,(fmove cd $ rotateLeft d),1)
      , (res,(fmove cd $ rotateRight d),1)
      ]
    newSeen = seen <> S.map (snd3 &&& thr3) toVisit

part1 :: Puzzle -> Int
part1 pm = bsp pm mempty ((0,((0,0),S),0),mempty)

part2 :: Puzzle -> Int
part2 = const 0

main = interact $ show . (part1 &&& part2 ) . parseInput