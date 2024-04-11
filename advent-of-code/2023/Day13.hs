import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (transpose, tails, inits, find)
import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)
import Control.Monad (join)
import Control.Exception

data Field = Dot | Hash deriving (Eq, Show)
type Pattern = [[Field]]
data Reflection = Horizontal Int | Vertical Int deriving (Eq, Show)

parseInput :: String -> [Pattern]
parseInput = map parsePattern . splitOn "\n\n"
  where
    parsePattern = map (map parseField) . lines
    parseField '.' = Dot
    parseField '#' = Hash

horizontal :: Pattern -> [Reflection]
horizontal p = map Horizontal . index . init . tail . zipWith isMirror (inits p) $ tails p
  where
    index = map fst . filter snd . zip [1..]
    isMirror a = and . zipWith (==) (reverse a)
    safeHead [] = Nothing
    safeHead (x:_) = Just x

vertical :: Pattern -> [Reflection]
vertical = map flipReflection . horizontal . transpose

reflection :: Pattern -> [Reflection]
reflection p = horizontal p <> vertical p

score :: Reflection -> Int
score (Horizontal x) = x * 100
score (Vertical x) = x

flipReflection :: Reflection -> Reflection
flipReflection (Horizontal x) = Vertical x
flipReflection (Vertical x) = Horizontal x

flipField :: Field -> Field
flipField Dot = Hash
flipField Hash = Dot

fixSmudge :: Pattern -> [Reflection]
fixSmudge p = filter (/= ref) . concatMap reflection $ smudge p
  where
    [ref] = reflection p

smudge :: Pattern -> [Pattern]
smudge p = map trySmudge coords
  where
    coords = [ (x,y) | y <- [0..length p - 1], x <- [0..length (head p) - 1] ]
    trySmudge (x,y) =
      let fixLine = map (\(sx,c) -> if sx == x then flipField c else c) . zip [0..]
      in map (\(sy,l) -> if sy == y then fixLine l else l) $ zip [0..] p

part1 :: [Pattern] -> Int
part1 = sum . map (score . head . reflection)

part2 :: [Pattern] -> Int
part2 = (`div` 2) . sum . map score . concatMap fixSmudge

main = interact $ show . (part1 &&& part2) . parseInput
