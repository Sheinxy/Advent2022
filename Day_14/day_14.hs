module Main where

import Data.Char
import Data.List
import qualified Data.Map as M (Map, filter, insert, fromList, size, elems, keys, notMember)

type Grid = M.Map (Int, Int) Char

parseLine :: String -> [((Int, Int), Char)]
parseLine line = concatMap createLine pairs
    where coordinates = map (read . (\x -> "(" ++ x ++ ")")) . filter (isDigit . head) . words $ line
          pairs = zip coordinates . tail $ coordinates
          createLine ((x1, y1), (x2, y2)) = [((x, y), '#') 
                                            | x <- [min x1 x2 .. max x1 x2], 
                                              y <- [min y1 y2 .. max y1 y2]]

addSand :: Int -> ((Int, Int) -> Grid -> Grid) -> Grid -> Grid
addSand maxValue onMax g = go (500, 0)
    where go (x, y) | y >= maxValue                = onMax (x, y) g
                    | M.notMember (x    , y + 1) g = go (x    , y + 1)
                    | M.notMember (x - 1, y + 1) g = go (x - 1, y + 1)
                    | M.notMember (x + 1, y + 1) g = go (x + 1, y + 1)
                    | otherwise                    = M.insert (x, y) 'o' g

main = do
    input <- M.fromList . concatMap parseLine . lines <$> readFile "input"
    let maxValue = maximum . map snd . M.keys $ input
    let fix f x = if x == f x then x else fix f (f x)
    print $ M.size . M.filter (== 'o') . fix (addSand maxValue (\_ g -> g)) $ input
    print $ M.size . M.filter (== 'o') . fix (addSand (maxValue + 1) (flip M.insert 'o')) $ input
