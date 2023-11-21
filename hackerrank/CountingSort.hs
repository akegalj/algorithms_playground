import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

count :: [Int] -> [Int]
count xs = V.toList $ foldl' (\acc x -> V.modify (\v -> MV.modify v (+ 1) x) acc) empty xs
  where
    empty = V.replicate 100 0

main = do
  _ <- getLine
  xs <- map read . words <$> getLine
  putStr . unwords . map show $ count xs