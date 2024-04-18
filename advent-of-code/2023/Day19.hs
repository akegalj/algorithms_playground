import Prelude hiding (Ordering (..))
import Control.Arrow ((&&&))
import qualified Data.Map as M
import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isNumber, toUpper)
import Control.Applicative ((<|>))

type Name = String

data Part = X | M | A | S deriving (Show, Read, Enum)
data Ordering = LT | GT deriving Show
data Result = Accepted | Rejected deriving (Show, Eq)
data SendTo = Dest Name | Result Result deriving (Show, Eq)
data Condition = Condition Part Ordering Int deriving Show
data Rule = Rule Condition SendTo deriving Show
newtype LastRule = LastRule SendTo deriving Show
data Workflow = Workflow Name [Rule] LastRule deriving Show
data Rating = Rating { x,m,a,s :: Int } deriving Show
type System = ([Workflow],[Rating])

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

parseInput :: String -> System
parseInput = fst . last . readP_to_S systemP
  where
    systemP = (,) <$> many (workflowP <* skipSpaces) <*> many (ratingP <* skipSpaces)
    workflowP = do
      n <- nameP
      (rs,lr) <- braces ((,) <$> (sepBy ruleP comma <* comma) <*> lruleP)
      pure $ Workflow n rs lr
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

workflow :: SendTo -> M.Map Name ([Rule],LastRule) -> Rating -> Result
workflow (Result r) _ _ = r
workflow (Dest n) wfs r = go $ wfs M.! n
  where
    go ([],LastRule st) = workflow st wfs r
    go ((Rule c st):rs,lr)
      | test c r = workflow st wfs r
      | otherwise = go (rs,lr)

part1 :: System -> Int
part1 (ws,rs) = sum . map fst . filter ((==Accepted) . snd) $ map (rating &&& workflow (Dest "in") wsMap) rs
  where
    wsMap = M.fromList $ map (\(Workflow n r l) -> (n,(r,l))) ws

part2 :: System -> Int
part2 = const 0

main = interact $ show . (part1 &&& part2) . parseInput