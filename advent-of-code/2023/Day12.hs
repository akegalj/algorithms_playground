{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}
import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import Data.List (groupBy, sort, nub, foldl', intersperse, find)
import qualified Data.Map as M
import Data.Function (on)
import Data.Bits (setBit, Bits, (.|.), shiftL, (.&.), testBit)
import Data.Maybe (fromMaybe)
import Debug.Trace

data Spring = Operational | Damaged | Unknown deriving (Eq, Show)
newtype GroupSize = GroupSize Int
  deriving stock (Eq, Ord, Show)
  deriving newtype Read
type ConRec = ([Spring], [GroupSize])
type MustUse = Bool

parseInput :: String -> [ConRec]
parseInput = map parseLine . lines
  where
    parseLine = (\[l,r] -> (map parseSpring l, map read $ splitOn "," r)) . words
    parseSpring '.' = Operational
    parseSpring '#' = Damaged
    parseSpring '?' = Unknown

indexGroups :: ConRec -> M.Map GroupSize [(MustUse, Int)]
indexGroups (xs, gs) = M.fromList . map (id &&& index) . nub $ sort gs
  where
    sm = springMask xs :: Integer
    lsm = length xs
    toDamaged = map (\x -> if x == Unknown then Damaged else x) xs
    contGroup = indexCont toDamaged
    touchesDamaged (s,i) = testBit sm (lsm-i-s-1) || testBit sm (lsm-i)
    constrainDamaged :: [(Int, Int)]
    constrainDamaged = indexCont xs
    index (GroupSize g) = concatMap (\(l, i) -> [(elem ix . map snd $ filter ((==g) . fst) constrainDamaged, ix) | ix <- [i..i+l-g], not $ touchesDamaged (g,ix)]) contGroup

    indexCont = map (length &&& fst . head) . filter (any $ (==Damaged) . snd) . groupBy ((==) `on` snd) . zip [0..]

springMask :: (Bits a, Num a) => [Spring] -> a
springMask = foldl' setBit 0 . map fst . filter ((==Damaged) . snd) . zip [0..] . reverse

indexMask :: (Bits a, Num a) => Int -> [(GroupSize, Int)] -> a
indexMask l = foldl' (.|.) 0 . map (\(GroupSize g,i) -> shiftL (2^g-1) $ l-i-g)

testMask :: Int -> ConRec -> [Int] -> Bool
testMask sm (sx, gs) ix = sm .&. indexMask (length sx) (zip gs ix) == sm

simulate :: ConRec -> [[Int]]
simulate cr@(xs, gs) = go 0 $ zip gs groupIxs
  where
    ig = indexGroups cr
    groupIxs = map (ig M.!) gs
    go :: Int -> [(GroupSize, [(MustUse, Int)])] -> [[Int]]
    go _ [] = [[]]
    go !start ((GroupSize l, is):gs) = concatMap (\(_,i) -> map (i:) $ go (i+l+1) gs) $ dropAfter fst $ dropWhile ((start>) . snd) is
    dropAfter _ [] = []
    dropAfter f (x:xs)
      | f x = x:[]
      | otherwise = x : dropAfter f xs

part1 :: [ConRec] -> Int
part1 = sum . map (\cr@(s,_) -> let sm = springMask s in length . filter (testMask sm cr) $ simulate cr)

part2 :: [ConRec] -> Int
part2 = part1 . map (\(a,b) -> trace (show a) $ expandInput b) . zip [0..]

expandInput (sx, gs) = (concat $ intersperse [Unknown] $ replicate 5 sx, concat $ replicate 5 gs)

main = interact $ show . (part1 &&& part2) . parseInput