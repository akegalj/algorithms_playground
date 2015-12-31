-- FIXME: unfinished
-- https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles

mainSierpinski :: [[Bool]]
mainSierpinski = mainSierpinski' 1
    where mainSierpinski' n | n > maxN = []
                            | otherwise = (zeroLine (maxN - n) ++ nonZeroLine (2*n - 1)  ++ zeroLine (maxN - n)) : mainSierpinski' (n + 1)
          zeroLine n = replicate n False
          nonZeroLine = map not . zeroLine
          maxN = 32

fractalization :: Int -> Int -> [[Bool]] -> [[Bool]]
fractalization u d xss =
    where (start, end) = (d - u + 1, d

sierpinski :: Int -> [[Bool]]
sierpinski n = fractalization 0 31 . mainSierpinski

showSierpinski :: [[Bool]] -> String
showSierpinski = unlines . map (map boolToSierpinski)
    where boolToSierpinski True  = '1'
          boolToSierpinski False = '_'

main :: IO ()
main = do
    input <- getLine
    putStr . showSierpinski . sierpinski $ stringToInt input
    where stringToInt = (read :: String -> Int)
