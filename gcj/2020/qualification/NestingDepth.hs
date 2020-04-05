{-# LANGUAGE TupleSections #-}
module NestingDepth where

import Control.Monad
import Data.Char
import Prelude

type Expression = String
type NestingExpression = String
type TestNum = Int
type TestCase = (TestNum, Expression)
type Output = (TestNum, NestingExpression)

nestingExpression :: Expression -> NestingExpression
nestingExpression ex = init . snd $ foldl computeNesting (0, "") (ex ++ "0")
  where
    computeNesting (height, newExp) digitC =
        let digit = digitToInt digitC
            climb = digit - height
            nesting =
                if climb > 0
                    then replicate climb '('
                    else replicate (negate climb) ')'
        in (digit, newExp ++ nesting ++ (show digit))

readExpression :: IO Expression
readExpression = getLine

computeOutput :: TestCase -> Output
computeOutput (tx, ex) =
    (tx, nestingExpression ex)

outputTestCase :: Output -> String
outputTestCase (tc, ne) =
    "Case #" ++ show tc ++ ": " ++ ne

outputAll :: [Output] -> IO ()
outputAll = putStr . unlines . map outputTestCase

inputAll :: IO [TestCase]
inputAll = do
    t <- read <$> getLine :: IO Int
    forM [1..t] $ \tc ->
        (tc,) <$> readExpression

main :: IO ()
main = do
    input <- inputAll
    outputAll $ map computeOutput input
