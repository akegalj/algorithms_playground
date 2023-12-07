{-# LANGUAGE ViewPatterns #-}
import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.List

data Card = A | K | Q | J | T | C9 | C8 | C7 | C6 | C5 | C4 | C3 | C2 deriving (Show, Eq, Ord, Enum)
type Hand = [Card]
data HandType = FiveKind | FourKind | FullHouse | ThreeKind | TwoPair | OnePair | HighCard deriving (Show, Eq, Ord, Enum)
type Game = [(HandType, Hand, Int)]

part1 :: Game -> Int
part1 = sum . map (\(r,(_,_,b)) -> r*b) . zip [1..] . reverse . sort
part2 = const 0

parse :: String -> Game
parse = map (parsePlayer . words) . lines
  where
    parsePlayer [cards, bid] = let hand = parseHand cards
                               in (parseHandType hand, hand, read bid)
    parseHand = map parseCard
    parseCard = (M.fromList (zip "AKQJT98765432" [A .. C2]) M.!)
    sortByCard = sortOn length . group . sort
    parseHandType (sortByCard -> hand)
      | length (last hand) == 5 = FiveKind
      | length (last hand) == 4 = FourKind
      | length (last hand) == 3 && length hand == 2 = FullHouse
      | length (last hand) == 3 = ThreeKind
      | length (filter ((==2) . length) hand) == 2 = TwoPair
      | length (filter ((==2) . length) hand) == 1 = OnePair
      | length hand == 5 = HighCard

main = interact $ show . (part1 &&& part2) . parse