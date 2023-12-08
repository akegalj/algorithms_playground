import Control.Arrow ((&&&))
import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.Map as M

data Dir = L | R deriving (Show, Read, Eq)
type Map = ([Dir], M.Map String (String, String))

part1, part2 :: Map -> Int
part1 (d,m) = fromJust . findIndex (=="ZZZ") . scanl (flip next) "AAA" $ cycle d
  where
    next L = fst . (m M.!)
    next R = snd . (m M.!)

part2 (d,m) = fromJust . findIndex (all $ isSuffixOf "Z") . scanl (flip next) startNodes $ cycle d
  where
    startNodes = filter (isSuffixOf "A") $ M.keys m
    next L = map $ fst . (m M.!)
    next R = map $ snd . (m M.!)

parse :: String -> Map
parse = (\[p,xs] -> (map (read . (:[])) p, parseMap xs)) . splitOn "\n\n"
  where
    parseMap = M.fromList . map parseNode . lines
    parseNode = (\[node,xs] -> (node, parseTupple xs)) . splitOn " = "
    parseTupple = (\[f,s] -> (f,s)) . splitOn ", " . tail . init

main = interact $ show . (part1 &&& part2) . parse