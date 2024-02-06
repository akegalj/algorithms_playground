import Data.Maybe
import Data.Bifunctor
import qualified Data.Set as S

solve :: (S.Set Int, Int, [[Int]]) -> [Int]
solve (hs, heroDefense, ds) = map slayDragon ds
  where
    search x = (S.lookupLT x hs, S.lookupGE x hs)
    slayDragon [def,att] =
      let (sc, nc) = bimap (nextCoins <$>) (strongCoins <$>) $ search def
          strongCoins sh = max 0 $ att - (heroDefense - sh)
          nextCoins nh = max 0 (att - (heroDefense - nh)) + (def - nh)
      in minimum $ catMaybes [sc, nc]

main = interact $ unlines . map show . solve . parse . map (map read . words) . lines
  where
    parse (_:hs:_:ds) = let k = sum hs
                            z = S.fromList hs
      in k `seq` z `seq` (z,k,ds)
