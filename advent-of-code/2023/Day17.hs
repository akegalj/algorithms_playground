import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Set as S
import Data.Bifunctor (first, second)
import Control.Monad.State
import Debug.Trace
import Data.List (find)
import Control.Monad (join)

data Direction = N | E | S | W deriving (Show, Eq, Enum, Ord)
type Coord = (Int, Int)
type Map = M.Map Coord Int
type ResMap = M.Map (Int, (Coord,Direction)) (Maybe Int)

parseInput :: String -> Map
parseInput = M.fromList . concatMap (\(y,l) -> map (\(x,c) ->((x,y), read [c])) l) . map (fmap $ zip [0..]) . zip [0..] . lines

rotateLeft, rotateRight :: Direction -> Direction
rotateLeft  d = (!!3) $ [d..W] <> [N .. W]
rotateRight d = (!!1) $ [d..W] <> [N .. W]

move :: Direction -> (Coord, Direction) -> (Coord, Direction)
move N = (,N) . second pred . fst
move S = (,S) . second succ . fst
move E = (,E) . first succ . fst
move W = (,W) . first pred . fst

showPath :: Map -> M.Map Coord Direction -> String
showPath m visited = unlines $ map (\y -> [ if (x,y) `M.member` visited then '#' else '.' | x <- [0..maxX]]) [0..maxY]
  where
    ((maxX, maxY),_) = M.findMax m

constructPath :: ResMap -> Int -> Coord -> [(Coord,Direction)]
constructPath rm i c = (c,direction) : constructPath rm iNext undefined -- (fst $ move direction (c, direction))
  where
    results :: [Maybe ((Int, (Coord, Direction)), Maybe Int)]
    results = map (\ix -> (ix,) <$> M.lookup ix rm) $ map (\d -> (i, (c,d))) [N .. W]
    smallRes :: Maybe Int
    smallRes = minJust $ map (join . fmap snd) results
    Just ((_,(_,direction)),_) = fromJust $ find ((==smallRes) . join . fmap snd) results
    iNext = undefined -- succ i

findPath :: Map -> State ResMap (Maybe Int)
findPath m = go' m 0 ((0,0),E)
  where
    (final, finalHeat) = M.findMax m
    (_, firstHeat) = M.findMin m
    go' :: Map -> Int -> (Coord,Direction) -> State ResMap (Maybe Int)
    go' m a b = do
      resm <- get
      case M.lookup (a,b) resm of
        Just res -> pure res
        Nothing -> do
          res <- go m a b
          modify' $ M.insert (a,b) res
          pure res
    go _ 3 _ = pure $ Nothing
    go m steps x@(coord,dir)
      | M.lookup coord m == Nothing = pure Nothing
      | coord == final = pure . Just $ finalHeat
      | otherwise = trace (show $ (steps,x)) $ do
          let m' = M.delete coord m
          dirs <- sequence $ map ($ dir)
                [ go' m' (succ steps) . flip move x
                , go' m' 0 . flip move x . rotateLeft
                , go' m' 0 . flip move x . rotateRight
                ]
          pure $ (+) <$> M.lookup coord m <*> minJust dirs

minJust :: (Ord a) => [Maybe a] -> Maybe a
minJust xs
  | all (==Nothing) xs = Nothing
  | otherwise = Just . minimum $ catMaybes xs

part1 :: Map -> Int
part1 = fromJust . flip evalState mempty . findPath

part2 :: Map -> Int
part2 = const 0

main = interact $ show . (part1 &&& part2 ) . parseInput