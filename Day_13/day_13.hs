module Main where

import Data.Char
import Data.List

data Packet = Number Int | List [Packet] deriving (Show, Read, Eq)

instance Ord Packet where
    compare (Number x) (Number y) = compare  x                y
    compare (Number x)         y  = compare (List [Number x]) y
    compare         x  (Number y) = compare  x               (List [Number y])
    compare (List x)   (List   y) = compare  x                y

parsePacket :: String -> Packet
parsePacket = read . go
    where go  []        = ""
          go ('[' : xs) = "List [" ++ go xs
          go ( x  : xs) | isDigit x = "Number " ++ (x : takeWhile isDigit xs) ++ go (dropWhile isDigit xs)
                        | otherwise = x : go xs

chunk :: Int -> [a] -> [[a]]
chunk n = takeWhile (not . null) . map (take n) . iterate (drop n)

main = do
    input <- map parsePacket . filter (not . null) . lines <$> readFile "input"
    print $ sum . map fst . filter snd . zip [1 .. ] . map (\[a, b] -> a <= b) . chunk 2 $ input
    let (div1, div2) = (List [List [Number 2]], List [List [Number 6]])
    let sorted = sort $ div1 : div2 : input
    let indexOf x = fst . head . filter ((== x) . snd) . zip [1 .. ]
    print $ indexOf div1 sorted * indexOf div2 sorted
