import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import qualified Data.Map as M

type Pulse = Bool
type Name = String
type Memory = M.Map Name Pulse
type Power = Bool
data Type = Broadcast | Conjuction Memory | FlipFlop Power deriving (Show, Eq)
type Modules = M.Map Name (Type,[Name])

isConjuction :: Type -> Bool
isConjuction (Conjuction _) = True
isConjuction _ = False

parseInput :: String -> Modules
parseInput = M.fromList . updateMemory . map parseLine . lines
  where
    parseLine line =
      let [l:ls,rs] = splitOn " -> " line
          ty = case l of
            'b' -> Broadcast
            '%' -> FlipFlop False -- off
            '&' -> Conjuction mempty -- init later
            _   -> error "boom"
          dest = splitOn ", " rs
          fixB xs = if ty == Broadcast then 'b':xs else xs
      in (fixB ls,(ty,dest))
    updateMemory ms =
      let conjs = map fst $ filter (isConjuction . fst . snd) ms
          conjMaps = M.fromList $ map (id &&& createMemory) conjs
          createMemory c = M.fromList . map ((,False) . fst) $ filter (elem c . snd . snd) ms
      in map (\all@(n,(_,ns)) -> if n `M.member` conjMaps then (n,(Conjuction $ conjMaps M.! n,ns)) else all) ms

part1 :: Modules -> Int
part1 = const 0

part2 :: Modules -> Int
part2 = const 0

main = interact $ show . (part1 &&& part2) . parseInput