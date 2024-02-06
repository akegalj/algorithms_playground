import Data.List

data Result = YES | NO deriving Show

toResult :: Bool -> Result
toResult True = YES
toResult False = NO

solve :: [Int] -> Bool
solve [a,b,c,m] = max' >= m && min' <= m
  where
    min' = foldr1 subtract (sort [a,b,c]) - 1
    max' = sum [a,b,c] - 3

main = interact $ unlines . map (show . toResult . solve . map read . words) . tail . lines