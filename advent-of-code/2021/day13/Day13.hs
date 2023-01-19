module Day13 where

import Data.List
import Data.List.Split
    ( chunksOf, splitOn )

data Fold = X | Y
    deriving (Show, Eq)

parseFold :: String -> Fold
parseFold "x" = X
parseFold "y" = Y

type Cave = (Coords, Folds)
type Folds = [(Fold, Int)]
type Coords = [(Int, Int)]

parseCave :: String -> Cave
parseCave i = (coords, folds)
  where
    c:f:[] = splitOn [""] $ lines i
    coords = map ((\(x:y:[]) -> (read x, read y)) . splitOn ",") c
    folds = map ((\(x:y:[]) -> (parseFold x, read y)) . splitOn "=" . drop 11) f

foldY :: Int -> Coords -> Coords
foldY y cave = nub . sort $ (up ++ down)
  where
    up = filter ((< y) . snd) cave
    down = map (\(x',y') -> (x', 2*y - y')) $ filter ((> y) . snd) cave

foldX :: Int -> Coords -> Coords
foldX x cave = nub . sort $ (left ++ right)
  where
    left = filter ((< x) . fst) cave
    right = map (\(x',y') -> (2*x - x', y')) $ filter ((> x) . fst) cave

foldCoord :: (Fold, Int) -> Coords -> Coords
foldCoord (X, x) = foldX x
foldCoord (Y, y) = foldY y

printCoords :: Coords -> String
printCoords coords = unlines $ chunksOf (maxX+1)
    [ if (x,y) `elem` coords then '#' else '.' | y <- [0..maxY], x <- [0..maxX]]
  where
    maxY = maximum $ map snd coords
    maxX = maximum $ map fst coords

main :: IO ()
main = do
    file <- readFile "input"
    print $ length $ (\(coords, f:_) -> foldCoord f coords) $ parseCave file
    putStrLn $ printCoords $ (\(coords, folds) -> foldl (flip foldCoord) coords folds) $ parseCave file