{-# LANGUAGE ViewPatterns #-}
import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.List

data HandType = FiveKind | FourKind | FullHouse | ThreeKind | TwoPair | OnePair | HighCard deriving (Show, Eq, Ord)
data CamelCard = CA | CK | CQ | CJ | CT | C9 | C8 | C7 | C6 | C5 | C4 | C3 | C2 deriving (Show, Eq, Ord, Enum, Bounded)
data JokerCamelCard = JA | JK | JQ | JT | J9 | J8 | J7 | J6 | J5 | J4 | J3 | J2 | JJ deriving (Show, Eq, Ord, Enum, Bounded)

part1 = solve . toGame @CamelCard (const False)
part2 = solve . toGame (==JJ)

solve :: Ord a => [(HandType, [a], Int)] -> Int
solve = sum . map (\(r,(_,_,b)) -> r*b) . zip [1..] . reverse . sort

parse :: String -> [(String, Int)]
parse = map ((\[c, b] -> (c, read b)) . words) . lines

toGame :: (Ord a, Eq a, Bounded a, Enum a, Show a) => (a -> Bool) -> [(String, Int)] -> [(HandType, [a], Int)]
toGame isJoker = map parsePlay
  where
    parsePlay (parseHand -> hand, bid) = (parseHandType hand, hand, bid)
    parseHand = map parseCard
    parseCard = (M.fromList (zip mapKeys cards) M.!)
    mapKeys = map (last . show) cards
    cards = [minBound .. maxBound]
    swapJoker r c = if isJoker c then r else c
    swapAllJokers hand = map (\r -> map (swapJoker r) hand) $ filter (not . isJoker) cards
    checkCond hand f = any f . map sortByCard . swapAllJokers $ hand
    sortByCard = sortOn length . group . sort
    parseHandType (checkCond -> check)
      | check $ (==5) . length . last = FiveKind
      | check $ (==4) . length . last = FourKind
      | check $ (==(3,2)) . (length . last &&& length) = FullHouse
      | check $ (==3) . length . last = ThreeKind
      | check $ (==2) . length . filter ((==2) . length) = TwoPair
      | check $ (==1) . length . filter ((==2) . length) = OnePair
      | check $ (==5) . length = HighCard

main = interact $ show . (part1 &&& part2) . parse