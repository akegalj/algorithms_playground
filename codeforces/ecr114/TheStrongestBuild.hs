import Data.List
import Data.Bifunctor
import qualified Data.Set as S
import Data.Function
import Control.Arrow
import qualified Data.Array.Unboxed as UA
import Data.Int

type Input = ([Int32], [UA.UArray Int32 Int32], [[Int32]])

solve :: Input -> [Int32]
solve (cs, slots, builds) = maximumBy (compare `on` strength) $ filter (flip S.notMember banned) combinations
  where
    combinations = sequence $ map (\x -> [1..x]) cs
    banned = S.fromList builds
    strength = sum . zipWith (UA.!) slots

parse :: String -> Input
parse i = (cs, slots, parseInts builds)
  where
    n:xs = lines i
    (cs, slots) = (map head &&& map (toArr . tail)) $ parseInts slots'
    (slots', _:builds) = splitAt (read n) xs
    parseInts = map $ map read . words
    toArr xs = UA.listArray (1 :: Int32, fromIntegral $ length xs) xs

main = interact $ unwords . map show . solve . parse