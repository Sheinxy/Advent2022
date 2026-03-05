module Main where

import Data.List
import Data.List.Split

parseInput :: String -> [[[Int]]]
parseInput = map (map parsePair . splitOn ",") . lines
    where
        interval [a, b] = [read a .. read b]
        parsePair = interval . splitOn "-"

fullyContains :: [[Int]] -> Bool
fullyContains l = foldl1 intersect l `elem` l

overlaps :: [[Int]] -> Bool
overlaps = not . null . foldl1 intersect

main = do
    input <- parseInput <$> readFile "input"
    print $ length . filter (== True) $ map fullyContains input
    print $ length . filter (== True) $ map overlaps input
