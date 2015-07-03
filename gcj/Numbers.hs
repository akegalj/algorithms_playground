{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)

data TestCase = Case CaseNum deriving (Show)
data Solution = Solution CaseNum
type CaseNum = Int

instance Show Solution where
  show (Solution cn) = "Case #" ++ show cn ++ ": "

readTests :: String -> [TestCase]
readTests = undefined

solve :: TestCase -> Solution
solve = undefined

main = do
  input <- getArgs >>= readFile . head
  mapM_ print . map solve $ readTests input
