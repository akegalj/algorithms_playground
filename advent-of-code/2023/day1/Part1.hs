import Data.Char

solve :: [String] -> Int
solve = sum . map (calibrate . filter isNumber)
  where
    calibrate s = read [head s, last s]

main = interact $ show . solve . lines