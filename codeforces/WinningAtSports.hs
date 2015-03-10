
import Data.Function (on)
import Data.Array

data TestCase = Case CaseNum Score deriving (Show)
data Solution = Solution CaseNum StressFree StressFull
type CaseNum = Int
type Score = (Win,Lose)
type Win = Int
type Lose = Int
type StressFree = Int
type StressFull = Int

instance Show Solution where
  show (Solution cn sf sf') = "Case #" ++ show cn ++ ": " ++ show sf ++ " " ++ show sf'

maxScore = 2000
moduloConst = 1000000007

-- | It could be implemented more efficient.
splitOn :: Eq a => a -> [a] -> ([a],[a])
splitOn a xs = (takeWhile (/=a) xs, tail $ dropWhile (/=a) xs)

readTests :: String -> [TestCase]
readTests = map readTest . zip [1..] . tail . lines
  where readTest :: (Int, String) -> TestCase
        readTest (cn,xs) = Case cn . uncurry ((,) `on` read) $ splitOn '-' xs

stressFree :: (Win,Lose) -> StressFree
stressFree = (sf!)
  where sf = array ((0,0),(maxScore,maxScore)) $ [((i,j), compSf i j `mod` moduloConst) | i <- [0..maxScore], j <- [0..i]]
        compSf 0 _ = 0
        compSf _ 0 = 1
        compSf i j = if i>j
                        then sf!(i-1,j) + sf!(i,j-1)
                        else 0

solve :: TestCase -> Solution
solve (Case cn s) = Solution cn (stressFree s) $ stressFree (minS+1, minS)
  where minS = min (fst s) (snd s)

main :: IO ()
main = do
  input <- getContents
  mapM_ print . map solve $ readTests input
