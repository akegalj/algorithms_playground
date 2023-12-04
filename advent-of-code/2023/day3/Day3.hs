import Control.Arrow ((&&&))
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)

part1 :: M.Map Point Char -> Int
part1 xs = sum $ M.elems partNumbers
  where
    symbols = M.keys $ M.filter isSymbol xs
    symbolNeighbours = foldMap neighbours symbols
    isSymAdjecent = not . S.disjoint symbolNeighbours
    partNumbers = M.filterWithKey (\k _ -> isSymAdjecent k) $ numbers xs
    isSymbol c = not $ isDigit c || c == '.'

part2 :: M.Map Point Char -> Int
part2 xs = sum $ map product gears
  where
    stars = M.keys $ M.filter isStar xs
    gears = filter ((== 2) . length) . map findPartNumbers $ map neighbours stars
    isAdjecent a = not . S.disjoint a
    findPartNumbers nh = M.elems . M.filterWithKey (\k _ -> isAdjecent nh k) $ numbers xs
    isStar c = c == '*'

neighbours :: Point -> S.Set Point
neighbours (x, y) = S.fromList [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]]

numbers :: M.Map Point Char -> M.Map (S.Set Point) Int
numbers xs = collectNums (0, 0) mempty mempty mempty
  where
    collectNums p@(0, py) num set res
      | M.notMember p xs = res
    collectNums p@(px, py) num set res =
      let addToRes f
            | null num = f mempty mempty res
            | otherwise = f mempty mempty (M.insert set (read $ reverse num) res)
       in case M.lookup p xs of
            Just c
              | isDigit c -> collectNums (px + 1, py) (c : num) (S.insert p set) res
              | otherwise -> addToRes $ collectNums (px + 1, py)
            Nothing -> addToRes $ collectNums (0, py + 1)

parse :: String -> M.Map Point Char
parse xs = M.fromList [((x, y), c) | (y, l) <- zip [0 ..] $ lines xs, (x, c) <- zip [0 ..] l]

main = interact $ show . (part1 &&& part2) . parse