module Day11 where

import Data.List
import Data.List.Split
import Data.Maybe

type Cave = [(Int, Int)]

parseCave :: String -> Cave
parseCave = concatMap (\(y, l) -> concatMap (\(x, n) -> replicate (read [n]) (y,x)) $ zip [0..] l) . zip [0..] . lines

maxX = 10
maxY = 10

step :: Cave -> (Cave, Int)
step cave = singleStep [] incAll
  where
    incAll = cave ++ [(y,x) | y <- [0..maxY-1], x <- [0..maxX-1]]
    singleStep fld c = let newNeighbours = concatMap (neighbours c) fl
                           fl = filter (`notElem` fld) $ flashCoords c
                       in if length fl == 0
                            then (filter (`notElem` fld) c, length . nub $ sort fld)
                            else singleStep (fld ++ fl) $ newNeighbours ++ c

flashCoords :: Cave -> [(Int, Int)]
flashCoords = map head . filter ((>=10) . length) . group . sort

neighbours :: Cave -> (Int, Int) -> [(Int, Int)]
neighbours cave (y, x) = [(fy y, fx x)| fy <- pos, fx <- pos, not $ fy 0 == 0 && fx 0 == 0, fy y >= 0 && fy y < maxY, fx x >= 0 && fx x < maxX]
  where
    pos = [pred, succ, id]

showCave :: Cave -> String
showCave cave = unlines $ chunksOf maxX $ concatMap show $ [ fromMaybe 0 (fmap snd $ find ((== (y,x)) . fst) c) | y <- [0..maxY-1], x <- [0..maxX-1]]
  where
    c = map (\g -> (head g, length g)) . group $ sort cave

main :: IO ()
main = do
    cave <- parseCave <$> readFile "input"
    let res = map snd $ scanr (const $ step . fst) (cave, 0) [0..10000]
    print $ sum $ take 100 $ reverse res
    print $ fst . head . filter ((==100) . snd) $ zip [0..] $ reverse res