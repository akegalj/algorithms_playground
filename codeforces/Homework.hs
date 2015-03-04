{-# LANGUAGE BangPatterns #-}
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import Control.Monad.Primitive
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

primacity :: PrimMonad m => Int -> m (VU.Vector Int)
primacity n = do
  v <- VUM.replicate (n + 1) 0
  forM_ [2..n `div` 2] $ \i -> do
    val <- VUM.unsafeRead v i
    when (val == 0) . forM_ [i, i*2 .. n] $ \ii -> do
      VUM.unsafeRead v ii >>= VUM.unsafeWrite v ii . (+1)
  VU.unsafeFreeze v

prims :: VU.Vector Int
prims = runST $ primacity (10^7)

solve :: TestCase -> Solution
solve (Case cn (a,b) k) = Solution cn . VU.length . VU.filter (== k) $ VU.slice a (b-a+1) prims

main = do
  input <- getContents
  mapM_ print . map solve $ readTests input

