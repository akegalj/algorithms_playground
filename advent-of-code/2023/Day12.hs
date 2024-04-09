{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}
import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import Data.List (groupBy, sort, nub, foldl', intersperse, find)
import qualified Data.Map as M
import Data.Function (on)
import Data.Bits (setBit, Bits, (.|.), shiftL, (.&.), testBit)
import Data.Maybe (fromMaybe, fromJust)
import Control.Applicative ((<|>))
import Debug.Trace

data Spring = Operational | Damaged | Unknown deriving (Eq, Show)
newtype GroupSize = GroupSize Int
  deriving stock (Eq, Ord, Show)
  deriving newtype (Read, Num)
type ConRec = ([Spring], [GroupSize])

parseInput :: String -> [ConRec]
parseInput = map parseLine . lines
  where
    parseLine = (\[l,r] -> (map parseSpring l, map read $ splitOn "," r)) . words
    parseSpring '.' = Operational
    parseSpring '#' = Damaged
    parseSpring '?' = Unknown

count :: GroupSize -> [Spring] -> [GroupSize] -> Maybe Int
count 0 [] [_] = Just 1
count _ [] _ = Nothing
count 0 (Operational:ss) gs = case gs of
  _:g':gs' -> count g' ss (g':gs') -- we pick next group
  _ -> if Damaged `elem` ss then Nothing else Just 1
count 0 (Damaged:ss) gs = Nothing
count 0 (Unknown:ss) gs = count 0 (Operational:ss) gs -- must be operational
count cur (Operational:ss) ags@(g:gs)
  | cur == g = count cur ss ags
  | otherwise = Nothing
count cur (Damaged:ss) gs = count (cur-1) ss gs
count cur (Unknown:ss) gs =
  let c1 = count cur (Operational:ss) gs
      c2 = count cur (Damaged:ss) gs
  in ((+) <$> c1 <*> c2) <|> c1 <|> c2
count a b c = error $ show (a,b,c)

part1 :: [ConRec] -> Int
part1 = sum . map (\(ss,g:gs) -> fromJust $ count g ss $ g:gs) -- FIXME

part2 :: [ConRec] -> Int
part2 = part1 . map (\(x,y) -> trace (show x) $ expandInput y) . zip [0..]

expandInput (sx, gs) = (concat $ intersperse [Unknown] $ replicate 5 sx, concat $ replicate 5 gs)

main = interact $ show . (part1 &&& part2) . parseInput