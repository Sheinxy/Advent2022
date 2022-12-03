module Main where

import Data.Char
import Data.List

getPriority :: Char -> Int
getPriority c
    | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
    | otherwise            = ord c - ord 'A' + 27

splitMiddle :: [a] -> ([a], [a])
splitMiddle l = splitAt (length l `div` 2) l

chunk :: Int -> [a] -> [[a]]
chunk n = takeWhile (not . null) . map (take n) . iterate (drop n)

priorityOfCommonItem :: (String, String) -> Int
priorityOfCommonItem = getPriority . head . uncurry intersect

priorityOfBadge :: [String] -> Int
priorityOfBadge = getPriority . head . foldl1 intersect

main = do
    input <- lines <$> readFile "input"
    print $ sum $ map (priorityOfCommonItem . splitMiddle) input
    print $ sum $ map priorityOfBadge $ chunk 3 input
