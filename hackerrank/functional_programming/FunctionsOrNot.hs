-- https://www.hackerrank.com/challenges/functions-or-not/problem?isFullScreen=false
import Control.Monad
    ( forM, mapM_ )
import Data.List
    ( nub, sort )

type Coord = (Int, Int)
data IsFunc = YES | NO
    deriving Show

parseInput :: IO [[Coord]]
parseInput = do
    n <- read <$> getLine
    forM [1..n] $ const $ do
        t <- read <$> getLine
        forM [1..t] $ const $ do
            (x:y:[]) <- words <$> getLine
            pure (read x,read y)

isFunction :: [(Int, Int)] -> IsFunc
isFunction xs = if length xs == (length $ unique xs)
    then YES
    else NO
  where
    unique = nub . sort . map fst

main :: IO ()
main = do
    mapM_ (print . isFunction) =<< parseInput