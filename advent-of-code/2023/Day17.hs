import Control.Arrow ((&&&))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor (first, second)

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
fmove = flip move

bfs :: (Steps -> Bool) -> (Steps -> Bool) -> (ICDS -> [ICDS]) -> Puzzle -> S.Set CDS -> (ICDS, S.Set ICDS) -> Int
bfs validFinish validStep nextSteps pm seen (icds@(res,cd@(c,d),s),xs)
  | c == target && validFinish s = res
  | otherwise = bfs validFinish validStep nextSteps pm newSeen . S.deleteFindMin $ toVisit <> xs
  where
    (target,_) = M.findMax pm
    fst3 (x,_,_) = x
    snd3 (_,x,_) = x
    thr3 (_,_,x) = x
    addRes (res,(c,d),s) = (res + pm M.! c,(c,d),s)
    validSteps = filter (validStep . thr3)
    validCoord = filter (\(_,cd@(c,_),s) -> (cd,s) `S.notMember` seen && c `M.member` pm)
    toVisit = S.fromList . map addRes . validCoord . validSteps $ nextSteps icds
    newSeen = seen <> S.map (snd3 &&& thr3) toVisit

part1 :: Puzzle -> Int
part1 pm = bfs (const True) (<4) nextSteps pm mempty ((0,((0,0),S),0),mempty)
  where
    nextSteps (res,cd@(c,d),s) =
      [ (res,(fmove cd d),succ s)
      , (res,(fmove cd $ rotateLeft d),1)
      , (res,(fmove cd $ rotateRight d),1)
      ]

part2 :: Puzzle -> Int
part2 pm = min (bfs' S) (bfs' E)
  where
    bfs' d = bfs (>3) (<11) nextSteps pm mempty ((0,((0,0),d),0),mempty)
    nextSteps (res,cd@(c,d),s)
      | s < 4 = [ (res,(fmove cd d),succ s) ]
      | otherwise =
          [ (res,(fmove cd d),succ s)
          , (res,(fmove cd $ rotateLeft d),1)
          , (res,(fmove cd $ rotateRight d),1)
          ]

main = interact $ show . (part1 &&& part2 ) . parseInput