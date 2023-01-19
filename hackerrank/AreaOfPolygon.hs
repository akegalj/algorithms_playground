{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- https://www.hackerrank.com/challenges/sherlock-and-anagrams
import           Data.List.Split (splitOn)

type Polygon = [Point]
data Point = Point Int Int
    deriving Show

data Triangle = Triangle Point Point Point
    deriving Show

newtype Distance = Distance Double
    deriving (Num, Fractional)

newtype Area = Area Double
    deriving Num

instance Show Distance where
    show (Distance a) = show a

instance Show Area where
    show (Area a) = show $ fromIntegral (round (a * 10)) / 10

triToPoly :: Triangle -> Polygon
triToPoly (Triangle a b c) = [a,b,c]

polyMesh :: Polygon -> [Triangle]
polyMesh (f:xs) = zipWith (Triangle f) (init xs) (tail xs)

distance :: Point -> Point -> Distance
distance (Point x1 y1) (Point x2 y2) = Distance $ sqrt $ (fromIntegral x2 - fromIntegral x1)^2 + (fromIntegral y2 - fromIntegral y1)^2

sides :: Polygon -> [Distance]
sides p = zipWith distance p (shiftPolygon p)

parimeter :: Polygon -> Distance
parimeter = sum . sides

semiparameter :: Triangle -> Distance
semiparameter = half . parimeter . triToPoly
  where
    half = (/ Distance 2)

heronArea :: Triangle -> Area
heronArea t = Area $ sqrt $ s * (s - a) * (s - b) * (s - c)
  where
    Distance s = semiparameter t
    [Distance a,Distance b,Distance c] = sides $ triToPoly t

area :: Polygon -> Area
area = sum . map heronArea . polyMesh

shiftPolygon :: Polygon -> Polygon
shiftPolygon (p:xs) = xs <> [p]

main :: IO ()
main = do
  input <- map ((\[a,b] -> Point (read a) (read b)) . splitOn " ") . tail . lines <$> getContents
  print $ area input
