import Data.Char
import Data.List

solve :: [String] -> Int
solve = sum . map (calibrate . filter isNumber . stringToNum)
  where
    calibrate s = read [head s, last s]

stringToNum :: String -> String
stringToNum [] = []
stringToNum str@(x : xs) = case mStartsWithNum str of
  Just i -> head (show i) : stringToNum xs
  Nothing -> x : stringToNum xs
  where
    nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    mStartsWithNum xs = (+ 1) <$> findIndex (flip isPrefixOf xs) nums

main = interact $ show . solve . lines