module Main where

import Data.List hiding (insert)
import Data.Map (Map, (!), fromList, insert, empty, member, size, filter, keys, adjust)

data Valve = Valve {rate :: Int, tunnels :: [String]} deriving (Show)
type Memo = Map (String, [String], Int) Int

parseLine :: String -> (String, Valve)
parseLine l = let w = words l in (w !! 1, Valve (read . init . drop 5 $ w !! 4) (map (Data.List.filter (/= ',')) . drop 9 $ w))

distances :: Map String Valve -> Map (String, String) Int
distances valves = foldl newd initd [(w, u, v) | w <- keys valves, u <- keys valves, v <- keys valves]
    where initv (u, v)     = if u == v then 0 else if v `elem` (tunnels (valves ! u)) then 1 else size valves
          initd            = fromList [((u, v), initv (u, v)) | u <- keys valves, v <- keys valves]
          newd d (w, u, v) = adjust (min (d ! (u, w) + d ! (w, v))) (u, v) d

flow :: Map String Valve -> Map (String, String) Int -> String -> Int -> [String] -> Memo -> (Int, Memo)
flow valves dist valve minutes closed memo | (valve, closed, minutes) `member` memo = (memo ! (valve, closed, minutes), memo)
                                           | otherwise = (best, insert (valve, closed, minutes) best m)
    where getRemaining t (u, v) = t - dist ! (u, v) - 1
          go (a, m) u           = let (u', m') = flow valves dist u (getRemaining minutes (valve, u)) (delete u closed) m 
                                  in (max a $ (rate $ valves ! u) * getRemaining minutes (valve, u) + u', m')
          (best, m)             = foldl go (0, memo) [u | u <- closed, dist ! (valve, u) < minutes]

flow2 :: Map String Valve -> Map (String, String) Int -> String -> Int -> [String] -> Memo -> (Int, Memo)
flow2 valves dist valve minutes closed memo = foldl go (flow valves dist "AA" 26 closed memo) [u | u <- closed, dist ! (valve, u) < minutes]
    where getRemaining t (u, v) = t - dist ! (u, v) - 1
          go (a, m) u           = let (u', m') = flow2 valves dist u (getRemaining minutes (valve, u)) (delete u closed) m 
                                  in (max a $ (rate $ valves ! u) * getRemaining minutes (valve, u) + u', m')
          reachable             = Data.List.filter (\u -> dist ! (valve, u) < minutes) closed

main = do
    input <- fromList . map parseLine . lines <$> readFile "input"
    let (dist, closed) = (distances input, keys . Data.Map.filter ((> 0) . rate) $ input)
    print $ fst $ flow  input dist "AA" 30 closed empty
    print $ fst $ flow2 input dist "AA" 26 closed empty
