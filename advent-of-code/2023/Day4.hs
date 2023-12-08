import Control.Arrow ((&&&))
import Data.List.Split
import qualified Data.Set as S

type Card = (S.Set Int, S.Set Int)

part1 :: [Card] -> Int
part1 = sum . map (\n -> 2 ^ (n - 1)) . filter (/= 0) . map (length . uncurry S.intersection)

part2 :: [Card] -> Int
part2 xs = copyCards (length xs) xs
  where
    copyCards num xs = sum $ map (\n -> go $ drop n xs) [0 .. num - 1]
    go [] = error "Impossible"
    go (x : xs) = isWin + copyCards winCards xs
      where
        winCards = length $ uncurry S.intersection x
        isWin = if winCards >= 0 then 1 else 0

parse :: String -> [Card]
parse = map (splitNums . map (S.fromList . map read . words) . splitOn "|" . (!! 1) . splitOn ":") . lines
  where
    splitNums [xs, ys] = (xs, ys)

main = interact $ show . (part1 &&& part2) . parse