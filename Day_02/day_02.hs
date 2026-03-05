module Main where

import Data.Char
import Data.List

parseInput :: String -> [(Int, Int)]
parseInput = map (\s -> (ord (s !! 0) - ord 'A', ord (s !! 2) - ord 'X')) . lines

playGame :: [Int] -> [Int] -> Int -> (Int, Int) -> Int
playGame ptsRes ptsAct cyc (p1, p2) = ptsAct !! p2 + cycle ptsRes !! (p1 + cyc * p2)

main = do
    input <- parseInput <$> readFile "input"
    print $ sum $ map (playGame [3, 0, 6] [1, 2, 3] 2) input
    print $ sum $ map (playGame [3, 1, 2] [0, 3, 6] 1) input
