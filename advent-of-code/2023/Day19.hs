import Prelude hiding (Ordering (..))
import Control.Arrow ((&&&))
import qualified Data.Map as M
import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isNumber, toUpper)
import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)

type Name = String

data Part = X | M | A | S deriving (Show, Read, Enum)
data Ordering = LT | GT deriving Show
data Result = Accepted | Rejected deriving (Show, Eq)
data SendTo = Dest Name | Result Result deriving (Show, Eq)
data Condition = Condition Part Ordering Int deriving Show
data Rule = Rule Condition SendTo deriving Show
newtype LastRule = LastRule SendTo deriving Show
type Workflows = M.Map Name ([Rule],LastRule)
data Rating = Rating { x,m,a,s :: Int } deriving Show
type System = (Workflows,[Rating])

rating :: Rating -> Int
rating r = sum $ map (getPart r) [X .. S]

getPart :: Rating -> Part -> Int
getPart (Rating x m a s) p = case p of
  X -> x
  M -> m
  A -> a
  S -> s

test :: Condition -> Rating -> Bool
test (Condition p LT n) r = getPart r p < n
test (Condition p GT n) r = getPart r p > n

notCond :: Condition -> Condition
notCond (Condition p LT n) = Condition p GT $ pred n
notCond (Condition p GT n) = Condition p LT $ succ n

parseInput :: String -> System
parseInput = fst . last . readP_to_S systemP
  where
    systemP = (,) <$> M.fromList <$> many (workflowP <* skipSpaces) <*> many (ratingP <* skipSpaces)
    workflowP = (,) <$> nameP <*> braces ((,) <$> (sepBy ruleP comma <* comma) <*> lruleP)
    ruleP = Rule <$> conditionP <*> (char ':' *> sendToP)
    lruleP = LastRule <$> sendToP
    conditionP = Condition <$> partP <*> orderingP <*> numP
    partP = read . map toUpper <$> nameP
    orderingP = (char '<' *> pure LT) <|> (char '>' *> pure GT)
    sendToP = (Result <$> (char 'A' *> pure Accepted <|> char 'R' *> pure Rejected)) <++ (Dest <$> nameP)
    ratingP = fmap (\[x,m,a,s] -> Rating x m a s) . braces $ sepBy (count 2 (satisfy $ const True) *> numP) comma
    braces = between (char '{') (char '}')
    numP = fmap read . many1 $ satisfy isNumber
    nameP = many1 $ satisfy isAlpha
    comma = char ','

workflow :: SendTo -> Workflows -> [[Condition]]
workflow st = catMaybes . mworkflow st
  where
    mworkflow (Result Rejected) _ = [Nothing]
    mworkflow (Result Accepted) _ = [Just []]
    mworkflow (Dest n) wfs = go $ wfs M.! n
      where
        go ([],LastRule st) = mworkflow st wfs
        go ((Rule c st):rs,lr) = concat [mfmap (c:) $ mworkflow st wfs, mfmap (notCond c:) $ go (rs,lr)]
        mfmap = map . fmap

part1 :: System -> Int
part1 (wfs,rs) = sum . map fst . filter snd $ map (rating &&& isAccepted) rs
  where
    isAccepted r = any (all (flip test r)) $ workflow (Dest "in") wfs

part2 :: System -> Int
part2 = undefined

main = interact $ show . (part1 &&& part2) . parseInput