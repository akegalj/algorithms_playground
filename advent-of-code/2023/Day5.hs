#!/usr/bin/env nix-shell
#!nix-shell --pure -i runhaskell -p "haskell.packages.ghc98.ghcWithPackages (pkgs: with pkgs; [ split ])"
{-# LANGUAGE BangPatterns #-}

import Control.Arrow ((&&&))
import Data.List.Split
import Control.Applicative
import Data.List

data Almanac = Almanac [Int] [Int -> Int]

part1 :: Almanac -> Int
part1 (Almanac seeds maps) = minimum $ map toLoc seeds
  where
    toLoc = foldl1' (flip (.)) maps

-- NOTE: Brute force!
part2 :: Almanac -> Int
part2 = part1 . fixAlmanac
  where
    fixAlmanac (Almanac seed maps) =
      Almanac (take 100000 . concatMap (\[s,r] -> [s..s+r-1]) $ splitEvery 2 seed) maps

parse :: String -> Almanac
parse xs = case splitOn "\n\n" xs of
  (s:xs) -> Almanac (parseSeed s) $ map parseMap xs
  where
    parseMap xs !s = maybe s id $ f xs s
    f xs !x = asum . map (check . map read . words) . tail . lines $ xs
      where
        check [dst, src, range]
          | src <= x && x < src + range = Just $ dst + x - src
          | otherwise = Nothing
    parseSeed = map read . tail . words

main = interact $ show . (part1 &&& part2) . parse