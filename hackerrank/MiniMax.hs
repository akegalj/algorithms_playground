solve :: [Int] -> [Int]
solve xs = [maxSum, minSum]
  where
    maxSum = maximum values
    minSum = minimum values
    values = [sum xs - picked | picked <- xs]

main = do
  [maxSum, minSum] <- solve . map read . words <$> getContents
  putStr $ show minSum ++ " " ++ show maxSum