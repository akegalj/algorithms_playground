import Control.Arrow ((&&&))
import Data.Char
import Data.List

part1 :: [String] -> Int
part1 = sum . map (calibrate . filter isNumber)
  where
    calibrate s = read [head s, last s]

part2 :: [String] -> Int
part2 = part1 . map stringToNum

stringToNum :: String -> String
stringToNum [] = []
stringToNum str@(x : xs) = case mStartsWithNum str of
  Just i -> head (show i) : stringToNum xs
  Nothing -> x : stringToNum xs
  where
    nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    mStartsWithNum xs = (+ 1) <$> findIndex (flip isPrefixOf xs) nums

main = interact $ show . (part1 &&& part2) . lines