{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Data.List (intersperse)

data Sentence = Sentence CaseNum [Word]
data ReversedSentence = RSentence CaseNum [Word]
type CaseNum = Int
type Word = String

instance Show ReversedSentence where
  show (RSentence cn xs) = "Case #" ++ show cn ++ ": " ++ unwords xs

readTests :: String -> [Sentence]
readTests = map createSentence . zip [1..] . tail . lines
  where createSentence :: (Int, String) -> Sentence
        createSentence (cn, xs) = Sentence cn $ words xs

solve :: Sentence -> ReversedSentence
solve (Sentence cn xs) = RSentence cn $ reverse xs

main = do
  input <- getArgs >>= readFile . head
  mapM_ print . map solve $ readTests input
