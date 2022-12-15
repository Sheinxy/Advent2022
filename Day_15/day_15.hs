module Main where

import Data.List

data Sensor = Sensor { position :: (Int, Int), closest :: (Int, Int), dist :: Int} deriving (Show, Eq)

parseSensor :: String -> Sensor
parseSensor s = Sensor (xs, ys) (xc, yc) (abs (xc - xs) + abs (ys - yc))
    where [_, _, x1, y1, _, _, _, _, x2, y2] = words s
          [xs, ys, xc, yc] = map (read . drop 2 . filter (not  . (`elem` ",:"))) [x1, y1, x2, y2]

reduceSortedInterval :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
reduceSortedInterval (a, b) (c, d) | c <= b = [(a, max b d)]
                                   | otherwise    = [(a, b), (c, d)]

coveringAtRow :: Int -> Int -> Int -> Sensor -> (Int, Int)
coveringAtRow boundMinX boundMaxX n (Sensor (x, y) _ d) = (max boundMinX minX, min boundMaxX maxX)
    where dy   = d - abs (n - y)
          minX = if dy <= 0 then 0 else x - dy
          maxX = if dy <= 0 then 0 else x + dy + 1

coverageAtRow :: Int -> Int -> Int -> [Sensor] -> [(Int, Int)]
coverageAtRow boundMinX boundMaxX n sensors = reduceAll coverage
    where coverage             = sort . filter (\(x, y) -> x /= y) . map (coveringAtRow boundMinX boundMaxX n) $ sensors
          fix f x              = if x == f x then x else fix f (f x)
          reduceTwo (x1:x2:xs) = reduceSortedInterval x1 x2 ++ xs
          reduceTwo  l         = l
          reduceAll []         = []
          reduceAll  l         = let (h:t) = fix reduceTwo l in h : reduceAll t

main = do
    input <- map parseSensor . lines <$> readFile "input" 
    let (minX, maxX) = (minimum . map (\s -> fst (position s) - dist s) $ input, maximum . map (\s -> fst (position s) + dist s + 1) $ input)
    let beaconsAtY = length . nub . filter ((== 2000000) . snd) . map closest $ input
    print $ foldl (\acc (a, b) -> acc + b - a) (-beaconsAtY) . coverageAtRow minX maxX 2000000 $ input
    let (((_, x):_), y) = head . dropWhile ((== 1) . length . fst) $ [(coverageAtRow 0 40000001 y input, y) | y <- [0 .. 4000000]]
    print $ 4000000 * x + y
