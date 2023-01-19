module Day17 where

import Data.List
import Data.List.Split
import Data.Maybe

parseTarget :: String -> ([Int], [Int])
parseTarget = (\[[x0,x1],[y0,y1]] -> ([read x0..read x1],[read y0..read y1])) . map (splitOn "..") . splitOn ", y=" . fromJust . stripPrefix "target area: x="

simulate :: ([Int], [Int]) -> Int -> (Int, Int) -> (Int, Int) -> Maybe Int
simulate t@(tx, ty) h (x, y) (vx, vy)
    | x `elem` tx && y `elem` ty = Just h
    | x > maximum tx || y < minimum ty = Nothing
    | otherwise = simulate t (max h y) (x+vx, y+vy) (max 0 $ vx-1, vy-1)

steps :: ([Int], [Int]) -> [Int]
steps t@(tx, ty) = catMaybes . map (simulate t 0 (0,0)) $ [(vx,vy)| vx <- [0..tx0], vy <- [ty0..negate ty0]]
  where
    tx0 = maximum tx
    ty0 = minimum ty

main :: IO ()
main = do
    target <- parseTarget <$> readFile "input"
    let res = steps target
    print $ maximum res
    print $ length res