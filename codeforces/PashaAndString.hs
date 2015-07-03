-- http://codeforces.com/contest/525/problem/B

import Data.Array.ST
import Data.Array.Base
import Control.Monad.ST

solve :: String -> [Int] -> String
solve s xs = runST $ do
  a <- newListArray (1,ls) s :: ST s (STUArray s Int Char)
  mapM_ (swap a) xs
  getElems a
    where ls = length s
          swap a i = do
            let j = ls - i + 1
            f <- readArray a i
            s <- readArray a j
            writeArray a j f
            writeArray a i s

main = do
  s <- getLine
  xs <- getLine >> getLine >>= return . map read . words
  putStrLn $ solve s xs

