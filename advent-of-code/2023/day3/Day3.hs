import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)

part1 :: M.Map Point Char -> Int
part1 xs = sum . M.keys $ M.filter (any isSymbol) numNeighbours
  where
    numNeighbours :: M.Map Int (S.Set Char)
    numNeighbours = M.map (\s -> S.map (xs M.!) $ (mconcat . map (allNeighbours xs M.!) $ S.toList s) S.\\ s) $ numbers xs
    isSymbol c = not $ isDigit c || c == '.'

allNeighbours :: M.Map Point Char -> M.Map Point (S.Set Point)
allNeighbours xs = M.mapWithKey (\p _ -> S.fromList $ neighbours p) xs
  where
    neighbours (px, py) = filter (flip M.member xs) [(px + x, py + y) | x <- [-1 .. 1], y <- [-1 .. 1], x /= 0 || y /= 0]

numbers :: M.Map Point Char -> M.Map Int (S.Set Point)
numbers xs = collectNums (0, 0) mempty mempty mempty
  where
    collectNums :: Point -> String -> S.Set Point -> M.Map Int (S.Set Point) -> M.Map Int (S.Set Point)
    collectNums p@(0, py) num set res
      | M.notMember p xs = res
    collectNums p@(px, py) num set res =
      let addToRes f
            | null num = f mempty mempty res
            | otherwise = f mempty mempty (M.insert (read $ reverse num) set res)
       in case M.lookup p xs of
            Just c
              | isDigit c -> collectNums (px + 1, py) (c : num) (S.insert p set) res
              | otherwise -> addToRes $ collectNums (px + 1, py)
            Nothing -> addToRes $ collectNums (0, py + 1)

parse :: String -> M.Map Point Char
parse xs = M.fromList [((x, y), c) | (y, l) <- zip [0 ..] $ lines xs, (x, c) <- zip [0 ..] l]

main = interact $ show . part1 . parse