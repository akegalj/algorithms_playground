module Day15 where

import Data.List
import Data.Maybe

import qualified Data.Map as M

type Cave = [[Int]]
type Edge = M.Map (Int, Int) Int

parseCave :: String -> Cave
parseCave = map (map $ read . pure) . lines

showCave :: Cave -> String
showCave = unlines . map (concatMap show)

goldCave :: Cave -> Cave
goldCave cave = fixLayout . map (map (\(f, xs) -> map (map (wrap.(+f))) xs) . uncurry zip) $ zip template replica
  where
    fixLayout = concatMap (\(c0:c1:c2:c3:c4:[]) -> zipWith5 (\c00 c11 c22 c33 c44 -> c00++c11++c22++c33++c44) c0 c1 c2 c3 c4)
    replica = replicate 5 $ replicate 5 cave
    l = length cave
    template = map (\n -> map (+n) [0..l-1]) [0..l-1]
    wrap n | n > 9 = n - 9
           | otherwise = n

shortestSum :: Cave -> Edge -> Edge -> Int
shortestSum cave edges seen
    | M.null edges = fromJust $ M.lookup (l - 1, l - 1) seen
    | otherwise = shortestSum cave (M.union (edges M.\\ removedEdges) newEdges) (M.union seen removedEdges)
  where
    l = length cave
    minRisk = minimum $ M.elems edges
    removedEdges = M.filterWithKey (const (== minRisk)) edges
    visited (c, _) = M.member c edges || M.member c seen
    newEdges = M.fromList . filter (not . visited) . concatMap neighbours $ M.keys removedEdges
    neighbours (y,x) = [((fy, fx), minRisk + cave!!fy!!fx) | (fy,fx) <- [(y-1,x),(y+1,x),(y,x-1),(y,x+1)], fy >= 0, fx >= 0, fy < l, fx < l]

main :: IO ()
main = do
    cave <- parseCave <$> readFile "input"
    print $ shortestSum cave (M.fromList [((0,0),0)]) mempty
    print $ shortestSum (goldCave cave) (M.fromList [((0,0),0)]) mempty