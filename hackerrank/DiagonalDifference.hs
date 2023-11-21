diagonalDifference :: [[Int]] -> Int
diagonalDifference xss = abs $ lr - rl
  where
    len = length xss
    lr = sum [xss !! i !! i | i <- [0 .. len - 1]]
    rl = sum [xss !! (len - i - 1) !! i | i <- [0 .. len - 1]]

main = do
  xss <- map (map read . words) . tail . lines <$> getContents
  print $ diagonalDifference xss