module Main where

import Data.Char
import Data.List

parseInput :: String -> ([String], [[Int]])
parseInput = parseLines . span (not . null) . lines
    where
        parseStacks = map head . takeWhile (not . null) . iterate (drop 4) . tail .
                      map (dropWhile (== ' ')) . transpose . init
        parseActions = map (map read . filter (all isDigit) . words) . tail
        parseLines (stk, act) = (parseStacks stk, parseActions act)

moveStacks :: Bool -> [String] -> [Int] -> [String]
moveStacks rev stacks [n, src, dst] = [
        if i == src then drop n s
        else if i == dst then getFromStack n src ++ s
        else s | (i, s) <- zip [1 .. ] stacks
    ]
    where getFromStack n src
            | rev = reverse . take n $ stacks !! (src - 1)
            | otherwise = take n $ stacks !! (src - 1)

main = do
    input <- parseInput <$> readFile "input"
    print $ map head . filter (not . null) $ uncurry (foldl $ moveStacks True) input
    print $ map head . filter (not . null) $ uncurry (foldl $ moveStacks False) input
