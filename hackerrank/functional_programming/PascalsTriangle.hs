-- https://www.hackerrank.com/challenges/pascals-triangle

memoizePascal :: Int -> [[Int]]
memoizePascal n = [[ pascal n x y | y <- [1..x]] | x <- [1..n]]

pascal :: Int -> Int -> Int -> Int
pascal _ 1 _ = 1
pascal _ _ 1 = 1
pascal n x y | x == y = 1
             | otherwise = memoizePascal'!!(x - 2)!!(y - 1) + memoizePascal'!!(x - 2)!!(y - 2)
               where memoizePascal' = memoizePascal n

printPascal :: [[Int]] -> IO ()
printPascal xss = do
    putStr . unlines . map unwords $ map (map show) xss

main :: IO ()
main = do
    input <- getLine
    printPascal . memoizePascal $ stringToInt input
    where stringToInt = (read :: String -> Int)
