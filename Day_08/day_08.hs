module Main where

import Data.Char
import Data.List

isVisible :: [[Int]] -> (Int, Int) -> Bool
isVisible grid (l, c) = fromLeft || fromTop || fromRight || fromBottom
    where 
        height     = grid !! l !! c
        line       = grid !! l
        column     = transpose grid !! c
        fromLeft   = all (< height) . take c $ line
        fromTop    = all (< height) . take l $ column
        fromRight  = all (< height) . drop (c + 1) $ line
        fromBottom = all (< height) . drop (l + 1) $ column

scenicScore :: [[Int]] -> (Int, Int) -> Int
scenicScore grid (l, c) = fromLeft * fromTop * fromRight * fromBottom
    where 
        height     = grid !! l !! c
        line       = grid !! l
        column     = transpose grid !! c
        numVisible = length . (\(f, s) -> f ++ if null s || head s > height then [] else [head s]) . span (< height)
        fromLeft   = numVisible . reverse $ take c line
        fromTop    = numVisible . reverse $ take l column
        fromRight  = numVisible $ drop (c + 1) line
        fromBottom = numVisible $ drop (l + 1) column

main = do
    input <- map (map digitToInt) . lines <$> readFile "input"
    let (lines, columns) = (length input, length $ input !! 0)
    print $ length . filter (== True) $ [isVisible input (x, y) | x <- [0 .. lines - 1], y <- [0 .. columns - 1]]
    print $ maximum [scenicScore input (x, y) | x <- [0 .. lines - 1], y <- [0 .. columns - 1]]
