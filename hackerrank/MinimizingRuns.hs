-- https://www.hackerrank.com/contests/101hack23/challenges/devu-and-minimizing-runs-of-a-sequence

import Data.List (group)
import qualified Data.Set as S

canMinimizeRuns :: String -> S.Set Char
canMinimizeRuns [_] = S.empty
canMinimizeRuns [_,_] = S.empty
canMinimizeRuns (x:y:z:rest) = if x == z && x /= y
                                  then y `S.insert` (canMinimizeRuns $ y:z:rest)
                                  else canMinimizeRuns $ y:z:rest

solve :: String -> Int
solve xs = length runs - lowerRuns
  where runs = group xs
        uniqueElements = S.fromList . concat $ filter ((==1) . length) runs
        lowerRuns = let minRuns = canMinimizeRuns xs
                        result | S.null minRuns = 0
                               | not . S.null $ S.intersection uniqueElements minRuns = 1
                               | otherwise = 2

                    in result
main = do
  input <- getContents
  mapM_ (print . solve) . tail $ lines input

