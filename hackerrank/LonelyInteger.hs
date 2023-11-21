import Data.List

uniqueElem :: [Int] -> Int
uniqueElem = head . head . filter ((== 1) . length) . group . sort

main = do
  xs <- map read . words . head . tail . lines <$> getContents
  print $ uniqueElem xs