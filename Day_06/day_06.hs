module Main where

import Data.Function
import Data.List

firstDifferent :: Int -> [(Int, Char)] -> [(Int, Char)]
firstDifferent n =  head . dropWhile ((/= n) . length) .
                    map (nubBy (on (==) snd) . take n) . iterate tail

main = do
    input <- zip [1 .. ] <$> readFile "input"
    print $ fst . last $ firstDifferent 4 input
    print $ fst . last $ firstDifferent 14 input
