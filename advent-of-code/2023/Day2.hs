#!/usr/bin/env nix-shell
#!nix-shell --pure -i runhaskell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ split ])"

import Control.Arrow ((&&&))
import Data.List
import Data.List.Split

type Red = Int

type Green = Int

type Blue = Int

type Set = (Red, Green, Blue)

type Game = [Set]

part1 :: [Game] -> Int
part1 = sum . map (+ 1) . findIndices (== True) . map (all legalMove)
  where
    legalMove (r, g, b) = r <= 12 && g <= 13 && b <= 14

part2 :: [Game] -> Int
part2 = sum . map (productOf3 . maxColors)
  where
    productOf3 (a, b, c) = a * b * c
    maxColors x = (maxColor fstOf3 x, maxColor sndOf3 x, maxColor thdOf3 x)
    maxColor c = maximum . map c
    fstOf3 (x, _, _) = x
    sndOf3 (_, x, _) = x
    thdOf3 (_, _, x) = x

parse :: String -> [Game]
parse = map (map (createSet . map words . splitOn ",") . splitOn ";" . head . tail . splitOn ":") . lines
  where
    createSet xs = (getColor "red" xs, getColor "green" xs, getColor "blue" xs)
    getColor c = maybe 0 (read . head) . find ((== c) . last)

main = interact $ show . (part1 &&& part2) . parse