import Data.List

data Bracket = B [Bracket] deriving Eq
newtype Solution = Solution [Bracket] deriving Eq

instance Show Solution where
  show (Solution b) = concatMap show b

instance Show Bracket where
  show (B lb) = "(" ++ concatMap show lb ++ ")"

mkBracket :: Int -> [Solution]
mkBracket 1 = [Solution [B []]]
mkBracket n =
  let prev = mkBracket (n - 1)
      insertBracket (Solution ps) = do
        n <- [0..length ps]
        let (l,r) = splitAt n ps
            between = Solution $ l ++ [B []] ++ r
            wrapL = Solution $ B l : r
            wrapR = Solution $ l ++ [B r]
        [between, wrapL, wrapR]
  in concatMap insertBracket prev

solveInput :: (Int, Int) -> [String]
solveInput (d, n) = take n . drop d . cycle . nub . map show $ mkBracket n

solve :: [Int] -> [[String]]
solve = map solveInput . zip [0..]

main = interact $ unlines . concat . solve . tail . map read . lines