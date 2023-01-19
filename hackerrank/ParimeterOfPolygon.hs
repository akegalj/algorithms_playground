{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- https://www.hackerrank.com/challenges/sherlock-and-anagrams
import           Data.List.Split (splitOn)

type Polygon = [Point]
data Point = Point Int Int
    deriving Show

newtype Distance = Distance Double
    deriving Num

instance Show Distance where
    show (Distance a) = show a

distance :: Point -> Point -> Distance
distance (Point x1 y1) (Point x2 y2) = Distance $ sqrt $ (fromIntegral x2 - fromIntegral x1)^2 + (fromIntegral y2 - fromIntegral y1)^2

parimeter :: Polygon -> Distance
parimeter p = sum $ zipWith distance p (shiftPolygon p)

shiftPolygon :: Polygon -> Polygon
shiftPolygon (p:xs) = xs <> [p]

main :: IO ()
main = do
  input <- map ((\[a,b] -> Point (read a) (read b)) . splitOn " ") . tail . lines <$> getContents
  print $ parimeter input
