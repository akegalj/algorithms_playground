{-# LANGUAGE BangPatterns #-}
-- http://codeforces.com/contest/525/problem/A

import Data.List (foldl')
import Data.Char (toLower)

-- Modified longest common substring problem with deletions.
{-# INLINE lcs' #-}
lcs' :: String -> String -> Int
lcs' s1 = last . foldl' pass (repeat 0)
  where pass !xs !c = tail . reverse . snd . foldl' (lcs c) (0,[0]) $! zip s1 xs
        lcs !s2c (!ul,!l) (!s1c,!u) = (u, maximum [u, head l, ul + if s1c == s2c then 1 else 0] : l)

main = do
  l <- (map toLower) `fmap` (getLine >> getLine)
  let filterPosition f = map snd . filter (f . fst) . zip [1..]
      l1 = filterPosition odd l
      l2 = filterPosition even l
  print . flip subtract (length l1) $ lcs' l1 l2

