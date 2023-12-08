import Control.Arrow ((&&&))
import Data.List.Split
import qualified Data.Map as M

data Dir = L | R deriving (Show, Read, Eq)
type Map = ([Dir], M.Map String (String, String))

part1, part2 :: Map -> Int
part1 (d,m) = go 0 "AAA" $ concat $ repeat d
  where
    go step "ZZZ" _ = step
    go step pos (L:xs) = go (step+1) (fst $ m M.! pos) xs
    go step pos (R:xs) = go (step+1) (snd $ m M.! pos) xs

part2 = const 0

parse :: String -> Map
parse = (\[p,xs] -> (map (read . (:[])) p, parseMap xs)) . splitOn "\n\n"
  where
    parseMap = M.fromList . map parseNode . lines
    parseNode = (\[node,xs] -> (node, parseTupple xs)) . splitOn " = "
    parseTupple = (\[f,s] -> (f,s)) . splitOn ", " . tail . init

main = interact $ show . (part1 &&& part2) . parse