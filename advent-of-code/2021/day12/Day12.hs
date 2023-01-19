module Day12 where

import Data.Char
    ( isLower, isUpper )
import Data.List
    ( nub, sort )
import Data.List.Split
    ( splitOn )

type Cave = [(String, String)]
type Edge = String

parseCave :: String -> Cave
parseCave = map ((\(a:b:[]) -> (a,b)) . splitOn "-") . lines

edges :: Cave -> [Edge]
edges = nub . sort . concatMap (\(a,b) -> [a,b])

lowerInnerEdges :: Cave -> [Edge]
lowerInnerEdges = filter (\e -> all isLower e && e /= "start" && e /= "end" ) . edges

findPath :: Maybe Edge -> Cave -> Edge -> Int
findPath bonus _ "end" = 1
findPath bonus [] _ = 0
findPath bonus cave edge
    | all isLower edge = sum $ map (findPath newBonus newCave) edges
    | otherwise = sum $ map (findPath bonus cave) edges
  where
    newBonus = if Just edge == bonus then Nothing else bonus
    newCave = if Just edge == bonus
                then cave
                else filter (\(a,b) -> a /= edge && b /= edge) cave
    edges = map (\(a,b) -> if a == edge then b else a) $ filter (\(a,b) -> a == edge || b == edge) cave

main :: IO ()
main = do
    cave <- parseCave <$> readFile "input"
    let silver = findPath Nothing cave "start"
        goldPart = map (\bonusEdge -> findPath (Just bonusEdge) cave "start") $ lowerInnerEdges cave
        gold = sum goldPart - (length goldPart - 1) * silver
    print silver
    print gold