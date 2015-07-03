
import Data.Array.Base
import Data.Array.ST

data TestCase = Case CaseNum Hierarchie
data Solution = Solution CaseNum Int

type CaseNum = Int
type Hierarchie = UArray Int Int

instance Show Solution where
  show (Solution cn s) = "Case #" ++ show cn ++ ": " ++ show s

readInput :: String -> [TestCase]
readInput = map readTest . filter (even . fst) . zip [1..] . tail . lines
  where readTest :: (CaseNum, String) -> TestCase
        readTest (cn, xs) = let list = map read $ words xs
                                in Case (cn `div` 2) $ listArray (0, length list) list

minGiftValue :: Hierarchie -> Int
minGiftValue h = sum . elems $ runSTUArray $ do
  let hb@(hl,hr) = bounds h
  mua <- newArray hb 0
  forM_ [hr,hr-1..hl] buyGift
    v <- readArray mua i
    when (v == 0)
  return mua

solve :: TestCase -> Solution
solve (Case cn h) = Solution cn $ minGiftValue h

main = do
  input <- getContents
  mapM_ print . map solve $ readInput input
