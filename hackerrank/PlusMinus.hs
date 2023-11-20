import Text.Printf

solve :: [Int] -> [Double]
solve xs = [poz / len, neg / len, zero / len]
  where
    len = fromIntegral $ length xs
    poz = fromIntegral . length $ filter (> 0) xs
    neg = fromIntegral . length $ filter (< 0) xs
    zero = fromIntegral . length $ filter (== 0) xs

main :: IO ()
main = do
  [poz, neg, zero] <- solve . map read . words . head . tail . lines <$> getContents
  printf "%.6f\n%.6f\n%.6f" poz neg zero