module Day10 where

import Data.List
import Data.Maybe

openBracket :: String
openBracket = "([{<"

closedBracket :: String
closedBracket = ")]}>"

findError :: String -> (String, Maybe Char)
findError = foldl insert ([], Nothing)
  where
    insert old@(_, Just _) _ = old
    insert (o:xs, _) c
        | o `elemIndex` openBracket == c `elemIndex` closedBracket = (xs, Nothing)
    insert (xs, _) b
        | b `elem` openBracket = (b:xs, Nothing)
        | otherwise = (xs, Just b)

score :: Char -> Int
score = ([3, 57, 1197, 25137]!!) . fromJust . flip elemIndex closedBracket

score2 :: String -> Int
score2 = foldl (\acc n -> acc*5 + n) 0 . map ((+1) . fromJust . flip elemIndex openBracket)

main :: IO ()
main = do
    file <- readFile "input"
    let processed =  map findError $ lines file
        median xs = sort xs !! (length xs `div` 2)
    print . sum $ map (score . fromJust) . filter isJust $ map snd processed
    print . median . map (score2 . fst) $ filter (isNothing . snd) processed