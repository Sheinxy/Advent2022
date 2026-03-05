module Main where

import Data.Char

type Resources = [Int]
type Robots    = [Int]
type Blueprint = [Resources]

parseBlueprint :: String -> Blueprint
parseBlueprint blueprint = [[a, 0, 0, 0], [b, 0, 0, 0], [c, d, 0, 0], [e, 0, f, 0]]
    where [a, b, c, d, e, f] = map read . filter (all isDigit) . words $ blueprint

nextStates :: Blueprint -> (Robots, Resources) -> [(Robots, Resources)]
nextStates bp (rb, rs) = reverse nextStates'
    where canBuildGeode = all (>= 0) $ zipWith (-) rs (last bp)
          canBuildObsi  = all (>= 0) $ zipWith (-) rs (bp !! 2)
          maxClay       = maximum $ map head $ take 2 bp
          indexedCosts  = zip [0 .. ] bp
          kDelta    i j = if i == j then 1 else 0
          deltaState    = [([kDelta i j | j <- [0 .. 3]], zipWith (-) rs costs) | (i, costs) <- indexedCosts]
          nextStates    = [(zipWith (+) rb dr, zipWith (+) rb drs) | (dr, drs) <- deltaState, all (>= 0) drs]
          nextStates'   = if canBuildGeode || canBuildObsi || (head rs >= maxClay) then nextStates else (rb, zipWith (+) rs rb):nextStates

dfs :: Blueprint -> Int -> Int -> (Robots, Resources) ->  Int
dfs bp 0 best (rb, rs) = max best $ last rs
dfs bp t best (rb, rs) | geodes + t * geodeBots + sum [i | i <- [1 .. t - 1]] <= best = best
                       | otherwise                                                    = foldl (dfs bp (t - 1)) best ns
    where geodes    = last rs
          geodeBots = last rb
          ns = nextStates bp (rb, rs)

main = do
    input <- map parseBlueprint . lines <$> readFile "input"
    let maxs = [dfs bp 24 0 ([1, 0, 0, 0], [0, 0, 0, 0]) | bp <- input]
    print $ sum [i * m | (i, m) <- zip [1 .. ] maxs]
    print $ product [(dfs bp 32 m ([1, 0, 0, 0], [0, 0, 0, 0])) | (m, bp) <- zip maxs $ take 3 input]
