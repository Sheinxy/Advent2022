module Main where

import Data.List

parseInput :: String -> [(Int, Int)]
parseInput = concatMap ((\[d, n] -> replicate (read n) $ getDirection d). words) . lines
    where getDirection "U" = ( 0,  1)
          getDirection "D" = ( 0, -1)
          getDirection "L" = (-1,  0)
          getDirection "R" = ( 1,  0)

newPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
newPos (x1, y1) (x2, y2)
    | (dx, dy) ==  (2, 2) = (nx, ny)
    |  dx == 2            = (nx, y1)
    |  dy == 2            = (x1, ny)
    |  otherwise          = (x2, y2)
    where (dx, dy) = (abs (x1 - x2), abs (y1 - y2))
          nx = if x1 > x2 then x1 - 1 else x1 + 1
          ny = if y1 > y2 then y1 - 1 else y1 + 1

move :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
move knots (dx, dy) = foldl (\nk p -> nk ++ [newPos (last nk) p]) [(nhx, nhy)] xs
    where (hx, hy):xs = knots
          (nhx, nhy) = (hx + dx, hy + dy)

main = do
    input <- parseInput <$> readFile "input"
    print $ length . nub . map last . scanl move [(0, 0), (0, 0)] $ input
    print $ length . nub . map last . scanl move (replicate 10 (0, 0)) $ input
