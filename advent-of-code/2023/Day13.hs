import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (transpose, tails, inits)
import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)

data Field = Dot | Hash deriving (Eq, Show)
type Pattern = [[Field]]
data Reflection = Horizontal Int | Vertical Int deriving (Eq, Show)

parseInput :: String -> [Pattern]
parseInput = map parsePattern . splitOn "\n\n"
  where
    parsePattern = map (map parseField) . lines
    parseField '.' = Dot
    parseField '#' = Hash

horizontal :: Pattern -> Maybe Reflection
horizontal p = fmap Horizontal . safeHead . index . init . tail . zipWith isMirror (inits p) $ tails p
  where
    index = map fst . filter snd . zip [1..]
    isMirror a = and . zipWith (==) (reverse a)
    safeHead [] = Nothing
    safeHead (x:_) = Just x

vertical :: Pattern -> Maybe Reflection
vertical = fmap (\(Horizontal x) -> Vertical x) . horizontal . transpose

reflection :: Pattern -> Maybe Reflection
reflection p = horizontal p <|> vertical p

score :: Reflection -> Int
score (Horizontal x) = x * 100
score (Vertical x) = x

part1 :: [Pattern] -> Int
part1 = sum . map score . catMaybes . map reflection

part2 :: [Pattern] -> Int
part2 = const 0

main = interact $ show . (part1 &&& part2) . parseInput
