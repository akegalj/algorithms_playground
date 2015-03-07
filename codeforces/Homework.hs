{-# LANGUAGE BangPatterns #-}
import Data.Array.ST
import Data.Array.Base
import Control.Monad.ST
import Control.Monad

data TestCase = Case CaseNum Range Primacity deriving (Show)
data Solution = Solution CaseNum PrimacityNumber
type CaseNum = Int
type Range = (Int, Int)
type Primacity = Int
type PrimacityNumber = Int

instance Show Solution where
  show (Solution cn pn) = "Case #" ++ show cn ++ ": " ++ show pn

readTests :: String -> [TestCase]
readTests = map readTest . zip [1..] . tail . lines
  where readTest :: (Int, String) -> TestCase
        readTest (cn,xs) = Case cn (a,b) k
          where (a:b:k:[]) = map read $ words xs

primacity :: Int -> UArray Int Primacity
primacity n = runSTUArray $ do
  v <- newArray (0,n) 0
  forM_ [2..n `div` 2] $ \i -> do
    val <- unsafeRead v i
    when (val == 0) . forM_ [i, i*2 .. n] $ \ii -> do
      unsafeRead v ii >>= unsafeWrite v ii . (+1)
  return v

-- | Implement accumulated sum (k <= 8) if speed is an issue
prims :: [Primacity]
prims = elems $ primacity (10^7)

solve :: TestCase -> Solution
solve (Case cn (a,b) k) = Solution cn . length . filter (== k) $ slice a (b-a+1) prims
  where slice s l = take l . drop s

main = do
  input <- getContents
  mapM_ print . map solve $ readTests input

