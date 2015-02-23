{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Data.List (intersperse)

data TestCase = Case CaseNum Credit [Price] deriving (Show)
type CaseNum = Int
type Credit = Int
type Price = Int

data Solution = Solution CaseNum Price Price

instance Show Solution where
  show (Solution c p1 p2) = "Case #" ++ show c ++ ": " ++ show p1 ++ " " ++ show p2

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy n xs = h:groupBy n t
  where (h,t) = splitAt n xs

readTests :: String -> [TestCase]
readTests i = map readTest . zip [1..read testNum] $ groupBy 3 tests
  where (testNum:tests) = lines i
        readTest :: (CaseNum, [String]) -> TestCase
        readTest (n,c:_:xs:[]) = Case n (read c :: Credit) (read $ "["++ map (\ a -> if a == ' ' then ',' else a) xs ++ "]" :: [Price])

solve :: TestCase -> Solution
solve (Case cn c xs) = uncurry (Solution cn) $ head [(min sp1 sp2,max sp1 sp2) | (fp1,sp1) <- zip xs [1..], (fp2,sp2) <- zip xs [1..], fp1 + fp2 == c, sp1 /= sp2]

main = do
  input <- getArgs >>= readFile . head
  mapM_ print . map solve $ readTests input


