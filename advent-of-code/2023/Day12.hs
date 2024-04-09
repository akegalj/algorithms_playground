{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}
import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import Data.List (groupBy, sort, nub, foldl', intersperse, find)
import qualified Data.Map as M
import Data.Function (on)
import Data.Bits (setBit, Bits, (.|.), shiftL, (.&.), testBit)
import Data.Maybe (fromMaybe)
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
count 0   [] [] = Just 1
count 0   [] [_] = Just 1
count 0   (Damaged:_) _ = Nothing
count 0   (Operational:ss) [_] = if Damaged `elem` ss then trace "woot" Nothing else Just 1
count 0   (Operational:ss) (_:g:gs) = count g ss gs
count cur (Operational:ss) ags@(g:_) = count g ss ags
count cur (Damaged:ss) ags = count (cur-1) ss ags
count cur (Unknown:ss) ags =
  let c1 = count cur (Operational:ss) ags
      c2 = count cur (Damaged:ss) ags
  in ((+) <$> c1 <*> c2) <|> c1 <|> c2
count a b _ = error $ show b

part1 :: [ConRec] -> Int
part1 = const 0

part2 :: [ConRec] -> Int
part2 = const 0 -- part1 . map expandInput

expandInput (sx, gs) = (concat $ intersperse [Unknown] $ replicate 5 sx, concat $ replicate 5 gs)

main = interact $ show . (part1 &&& part2) . parseInput