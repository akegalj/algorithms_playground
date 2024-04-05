import Control.Arrow ((&&&))
import Data.Bits
import Data.List
import Data.List.Split (splitOn)
import Debug.Trace

type RecMask = Int
type RecIx = [Int]
type RecRes = [Int]
type ConRec = ((RecMask, RecIx), RecRes)
type BitNum = [Bool]

fromBits :: Bits a => a -> BitNum
fromBits = reverse . go
  where
    go b
      | b == zeroBits = []
      | otherwise     = testBit b 0 : go (shiftR b 1)

groupSizes :: Bits a => a -> RecRes
groupSizes = map length . filter (any (==True)) . group . fromBits

parseInput :: String -> [ConRec]
parseInput = map parseLine . lines
  where
    parseLine = (\[pattern, sx] -> ((mask pattern, indexUnknown pattern), map read $ splitOn "," sx)) . words
    mask = foldl' setBit 0 . index '#'
    indexUnknown = index '?'
    index c = map fst . filter ((==c) . snd) . zip [0..] . reverse

fromRecIx :: (Bits a, Num a) => RecIx -> a -> a
fromRecIx [] 0 = 0
fromRecIx _  0 = 0
fromRecIx [] _ = error "boom"
fromRecIx (i:ix) b
  | testBit b 0 = fromRecIx ix (shiftR b 1) `setBit` i
  | otherwise = fromRecIx ix (shiftR b 1)

findSizes :: (Bits a, Num a) => (a, RecIx) -> a -> (a, RecRes)
findSizes (mask, ix) = (id &&& groupSizes) . (mask .|.) . fromRecIx ix

findValidArrangements :: ConRec -> [Int]
findValidArrangements ((mask,ix),res) = map fst . filter ((res==) . snd) $ map (findSizes (mask,ix)) [0..2^(length ix) - 1]

part1 :: [ConRec] -> Int
part1 = sum . map (length . findValidArrangements)

part2 :: [ConRec] -> Int
part2 = const 0

main = interact $ show . (part1 &&& part2) . parseInput