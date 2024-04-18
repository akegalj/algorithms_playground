import Control.Arrow ((&&&))
import Data.Char (ord, isAlpha, isNumber)
import Data.List (foldl')
import qualified Data.Map as M
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

newtype Hash = Hash { getHash :: Int } deriving (Eq, Ord, Show)
newtype FocalLength = FocalLength Int deriving (Eq, Show)
newtype Label = Label String deriving (Eq, Show)
data Lens = Lens Label FocalLength deriving (Show, Eq)
data Operation = Remove Label | Insert Lens deriving Show

type Box = [Lens]
type Book = M.Map Hash Box

labelP = Label <$> many (satisfy isAlpha) -- should be isAscii
focalLengthP = FocalLength <$> read <$> many (satisfy isNumber)
lensP = Lens <$> labelP <*> (char '=' *> focalLengthP)

operationP :: ReadP Operation
operationP = removeP <|> insertP
  where
    removeP = Remove <$> labelP <* char '-'
    insertP = Insert <$> lensP

parseInput :: String -> [Operation]
parseInput = fst . last . readP_to_S (sepBy operationP $ char ',')

hash :: Operation -> Hash
hash (Remove (Label l)) = runHash $ l <> "-"
hash (Insert (Lens (Label l) (FocalLength fl))) = runHash $ l <> "=" <> show fl

labelHash :: Operation -> Hash
labelHash (Remove (Label l)) = runHash l
labelHash (Insert (Lens (Label l) _)) = runHash l

runHash :: String -> Hash
runHash = Hash . foldl' (\h c -> (h + ord c)*17 `rem` 256) 0

runOperation :: Book -> Operation -> Book
runOperation m op@(Remove l) = M.update (Just . filter (\(Lens oldl _) -> l /= oldl)) (labelHash op) m
runOperation m op@(Insert l) = M.insertWith ins (labelHash op) [l] m
  where
    ins newl [] = newl
    ins [newl@(Lens l1 _)] (oldl@(Lens l2 _):xs)
      | l1 == l2 = newl:xs
      | otherwise = oldl:ins [newl] xs

focusingPower :: Book -> [Int]
focusingPower = concatMap (\(Hash k,xs) -> map (\(i,l) -> succ k * i * getFL l) $ zip [1..] xs ) . M.assocs
  where
    getFL (Lens _ (FocalLength l)) = l

part1 :: [Operation] -> Int
part1 = sum . map (getHash . hash)

part2 :: [Operation] -> Int
part2 = sum . focusingPower . foldl' runOperation mempty

main = interact $ show . (part1 &&& part2) . parseInput