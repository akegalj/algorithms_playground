#!/usr/bin/env nix-shell
#!nix-shell --pure -i runhaskell -p "haskell.packages.ghc98.ghcWithPackages (pkgs: with pkgs; [ split ])"

import Control.Arrow ((&&&))
import Data.List.Split
import Control.Applicative
import Data.List

data Skip = Skip | NoSkip deriving (Show, Eq, Ord)
type Tag a = (Skip, a)
data Almanac = Almanac [Int] [Tag Int -> Tag Int]

part1 :: Almanac -> Int
part1 = minimum . map snd . locs

part2 :: Almanac -> Int
part2 = const 0 -- minimum . map snd . ((<>) <$> lefts <*> rights) . locs . fixAlmanac
  where
    fixAlmanac (Almanac seed maps) =
      Almanac (sort . concatMap (\[s,r] -> [s..s+r-1]) $ splitEvery 2 seed) maps
    lefts = take 1 . filter ((==Skip) . fst)
    rights = take 1 . filter ((==NoSkip) . fst)

locs :: Almanac -> [Tag Int]
locs (Almanac seeds maps) = map toLoc seeds
  where
    toLoc = foldr1 (flip (.)) maps . (NoSkip,)

parse :: String -> Almanac
parse xs = case splitOn "\n\n" xs of
  (s:xs) -> Almanac (parseSeed s) $ map parseMap xs
  where
    f xs x = asum . map (check . map read . words) . tail . lines $ xs
      where
        check [dst, src, range]
          | src <= x && x < src + range = Just $ dst + x - src
          | otherwise = Nothing
    parseSeed = map read . tail . words
    parseMap xs s@(_,x) = maybe s (Skip,) $ f xs x

main = interact $ show . (part1 &&& part2) . parse