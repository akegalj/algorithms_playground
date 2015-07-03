{-# LANGUAGE BangPatterns #-}
-- https://www.hackerrank.com/challenges/common-child

import Data.List (foldl')
import Data.Array
import qualified Data.ByteString as B
import Debug.Trace (trace)

solve :: B.ByteString -> B.ByteString -> Int
solve s1 s2 = longestCommonSubs ! (lengthS1, lengthS2)
  where lengthS1 = B.length s1
        lengthS2 = B.length s2
        longestCommonSubs = array ((0,0),(lengthS1,lengthS2)) [ ((i,j), lcs i j) | i <- [1..lengthS1], j <- [1..lengthS2] ]
        lcs i j = let lcs' 0 _ = 0
                      lcs' _ 0 = 0
                      lcs' a b = longestCommonSubs!(a,b)
                  in maximum [ lcs' (i-1) j
                             , lcs' i (j-1)
                             , lcs' (i-1) (j-1) + if B.index s1 (i-1) == B.index s2 (j-1) then 1 else 0
                             ]

-- This version doesnt build N^2 thunks, also more strict.
{-# INLINE solve' #-}
solve' :: String -> String -> Int
solve' s1 = last . foldl' pass (repeat 0)
  where pass !xs !c = tail . reverse . snd . foldl' (lcs c) (0,[0]) $! zip s1 xs
        lcs !s2c (!ul,!l) (!s1c,!u) = (u, maximum [u, head l, ul + if s1c == s2c then 1 else 0] : l)

main = do
  l1 <- getLine
  l2 <- getLine
  print $ solve' l1 l2
