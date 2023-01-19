module Day14 where

import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe

type Template = String
type Rules = M.Map String Char

parseFile :: String -> (Template, Rules)
parseFile i = (p, splitPairs xs)
  where
    (p:_:xs) = lines i
    splitPairs = M.fromList . map (\(f:s:_) -> (f,head s)) . map (splitOn " -> ")

getPairs :: Template -> [String]
getPairs (_:[]) = []
getPairs (a:b:xs) = (a:b:[]):getPairs (b:xs)

step :: Template -> Rules -> String
step t rules = catMaybes . concatMap (\(c,mi) -> [Just c, mi]) . zip t . (++[Nothing]) . map (flip M.lookup rules) $ getPairs t

scoreSilver :: String -> Int
scoreSilver i = (length (last tmp) - length (head tmp))
  where
    tmp = sortOn length . group $ sort i

countPairs :: [String] -> M.Map String Int
countPairs = foldr (\k acc -> M.insertWith (+) k 1 acc) mempty

stepGold :: Rules -> M.Map String Int -> M.Map String Int
stepGold rules count = M.foldrWithKey (\k count acc -> insertCounts k count acc) mempty count
  where
    insertCounts key keyCount acc =
        let l:r:[] = getPairs $ step key rules
            insert k acc = M.insertWith (+) k keyCount acc
        in insert l $ insert r acc

scoreGold :: Char -> M.Map String Int -> Int
scoreGold fix count = (last tmp - head tmp)
  where
    fixLast same@(c, s)
        | c == fix = (c, s + 1)
        | otherwise = same
    tmp = sort . map (snd . fixLast) . map (\xs -> (fst (head xs), sum $ map snd xs)) . groupBy ((==) `on` fst) . sortOn fst . map (\(s,c) -> (head s, c)) $ M.toList count

main :: IO ()
main = do
    (t, rules) <- parseFile <$> readFile "input"
    print $ scoreSilver $ foldl (\t _ -> step t rules) t [0..9]
    print $ scoreGold (last t) $ foldl (\t _ -> stepGold rules t) (countPairs $ getPairs t) [0..39]