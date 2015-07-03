{-# LANGUAGE BangPatterns #-}
-- https://www.hackerrank.com/challenges/separate-the-chocolate
-- TODO: not solved
import Debug.Trace (trace)

data Player = T | D | U deriving (Eq, Show, Read)
type ChocolateRow = [Player]
type Chocolate = [ChocolateRow]

toChocolate :: String -> Chocolate
toChocolate = map (map $ read . (:[])) . lines

solve :: Chocolate -> Int
solve [] = 0
solve (c:cx) = buildChocolate cx (zip c $ repeat U) [U]

buildChocolate :: Chocolate -> [(Player,Player)] -> ChocolateRow -> Int
buildChocolate [] [] _ = 1
buildChocolate !cx [] !newRow = buildChoc cx [] newRow
buildChocolate !cx !prevRow !newRow = buildChoc cx prevRow (T:newRow)
                                    + buildChoc cx prevRow (D:newRow)

buildChoc :: Chocolate -> [(Player,Player)] -> ChocolateRow -> Int
buildChoc !(c:cx) [] !dx = buildChocolate cx (zip (reverse dx) (reverse $ tail dx)) [U]
buildChoc !cx !(u:ux) !dx@(dr:dl:_) | isValidChocolate u (dl,dr) = buildChocolate cx ux dx
                                    | otherwise = 0

isValidChocolate :: (Player,Player) -> (Player,Player) -> Bool
isValidChocolate (T,D) (D,T) = False
isValidChocolate (D,T) (T,D) = False
isValidChocolate (T,T) (T,T) = False
isValidChocolate (D,D) (D,D) = False
isValidChocolate _ _         = True

main = do
  k <- (read . last . words) `fmap` getLine :: IO Int
  input <- getContents
  print . solve $ toChocolate input
