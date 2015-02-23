{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.List (findIndex)

data TestCase = Case CaseNum Message
data Solution = Solution CaseNum T9Message

type CaseNum = Int
type Message = String
type Key     = Int
type KeyPos  = Int
data Symbol  = Symbol { key :: Key
                      , keyPos ::KeyPos } deriving (Show)

-- Nothing would indicate waiting (end of sequence)
type T9Message = [Maybe Symbol]

instance Show Solution where
  show (Solution cn xs) = "Case #" ++ show cn ++ ": " ++ concatMap t9ToString xs
    where t9ToString :: Maybe Symbol -> String
          t9ToString Nothing = " "
          t9ToString (Just s) = concat . replicate (keyPos s) . show $ key s

readTests :: String -> [TestCase]
readTests = map createTest . zip [1..] . tail . lines
  where createTest :: (Int, String) -> TestCase
        createTest (cn,m) = Case cn m

solve :: TestCase -> Solution
solve (Case cn m) = Solution cn $ messageToT9 m

symbolBucket = [" ", "", "abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz"]

messageToT9 :: Message -> T9Message
messageToT9 = insertPauses . map charToSymbol
  where insertPauses :: [Symbol] -> T9Message
        insertPauses xs = (++[Just $ last xs]) . concatMap (\(f,s) -> if key f == key s then [Just f,Nothing] else [Just f]) . zip xs $ tail xs
        charToSymbol :: Char -> Symbol
        charToSymbol c = Symbol k (kp+1)
          where (k,kp) = (\(k',p) -> (k',fromJust $ findIndex (==c) p)) . head . filter (elem c . snd) $ zip [0..] symbolBucket

main = do
  input <- getArgs >>= readFile . head
  mapM_ print . map solve $ readTests input
