{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy as BS
import Data.List.Split (splitWhen)
import Data.Char (isNumber)
import GHC.Word (Word8(..))
import qualified Data.Map as M
import Debug.Trace (trace)

data TestCase = Case CaseNum [BS.ByteString]
data Solution = Solution CaseNum KeyStrokeNum
type KeyStrokeNum = Int
type CaseNum = Int

data Tree a = Node    { keyStrokes :: Int
                      , takeForest :: Forest a } deriving (Show)
type Forest a = M.Map a (Tree a)

emptyForest :: Forest a
emptyForest = M.empty

instance Show Solution where
  show (Solution cn ksn) = "Case #" ++ show cn ++ ": " ++ show ksn

readTests :: BS.ByteString -> [TestCase]
readTests = map createTest . zip [1..] . splitWhen isNum . drop 2 . BSC.lines
  where isNum      = BSC.all isNumber
        createTest = uncurry Case

minKeyStrokes :: Int -> Forest Word8 -> BS.ByteString -> Forest Word8
minKeyStrokes inc forest word = if BS.null word
                               then forest
                               else case M.lookup (BS.head word) forest of
                                       Just a -> insertNode
                                       Nothing -> minKeyStrokes 0 insertNode word
  where updateNode :: Int -> Tree Word8 -> Tree Word8 -> Tree Word8
        updateNode inc _ (Node ks f) = Node (ks+inc) . minKeyStrokes inc f $ BS.tail word
        insertNode = M.insertWith (updateNode inc) (BS.head word) (Node inc emptyForest) forest

toList :: Forest a -> [Tree a]
toList forest = concat $ if M.null forest
                   then []
                   else f2List : map (toList . takeForest) f2List
  where f2List = map snd $ M.toList forest

solve :: TestCase -> Solution
solve (Case cn xs) = Solution cn . sum . map keyStrokes . toList $ foldl (minKeyStrokes 1) emptyForest xs

main = do
  input <- BS.getContents
  mapM_ print . map solve $ readTests input
