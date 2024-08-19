import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.Graph
import Data.List
import Data.Function

type Name = String

parseInput :: String -> Graph
parseInput = fstOf3 . graphFromEdges . addReverse . map ((\(x:xs) -> (init x, init x, xs)) . words) . lines
  where
    fstOf3 (x,_,_) = x
    thrOf3 (_,_,x) = x
    addReverse xs =
      let rev = map (\all@((i,_,_):_) -> (i,i,concatMap thrOf3 all)) . groupBy ((==) `on` fstOf3) $ concatMap (\(a,b,c) -> map (\i -> (i,i,[a])) c) xs
      in xs <> rev

part1 :: Graph -> Int
part1 = undefined

part2 :: Graph -> Int
part2 = const 0

main = interact $ show . (part1 &&& part2) . parseInput