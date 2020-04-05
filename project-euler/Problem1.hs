-- Multiply of 3 and 5
-- TODO: add task explanation and task link
module Problem1 where

-- TODO: add method specification
muls :: Int -> Int -> [Int]
muls step limit =
    map (*step) [1..((limit - 1) `div` step)]

main :: IO ()
main =
    let limit = 1000
    in print $ sum $ muls 3 limit ++ muls 5 limit
