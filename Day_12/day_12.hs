module Main where

import Data.Char
import Data.Map (Map, (!), update, insert, member, notMember, fromList, singleton)

type Grid    = Map (Int, Int) Int
type ParentV = Map (Int, Int) (Int, Int)

parseInput :: String -> ((Int, Int), (Int, Int), Grid)
parseInput input = (start, end, grid)
    where gridList   = concat [ map (\(c, x) -> ((r, c), x)) . zip [0 .. ] $ line | (r, line) <- zip [0 .. ] $ lines input ]
          getVal 'S' = 0
          getVal 'E' = ord 'z' - ord 'a'
          getVal  x  = ord  x  - ord 'a'
          start      = fst . head . filter ((== 'S') . snd) $ gridList
          end        = fst . head . filter ((== 'E') . snd) $ gridList
          grid       = fromList . map (\(p, x) -> (p, getVal x)) $ gridList

bfs :: ((Int, Int) -> Bool) -> [(Int, Int)] -> (Int -> Int -> Bool) -> Grid -> ParentV -> ((Int, Int), ParentV)
bfs _ [] _ _ parentv = ((-1, -1), parentv)
bfs isEnd ((r, c):xs) valid grid parentv
    | isEnd (r, c)   = ((r, c), parentv)
    | otherwise      = bfs isEnd queue valid grid parentv'
    where isValidNeighbour n = member n grid && notMember n parentv && valid (grid ! (r, c)) (grid ! n)
          neighbours = filter isValidNeighbour [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
          queue      = xs ++ neighbours
          parentv'   = foldl (\m k -> insert k (r, c) m) parentv neighbours

reconstructPath :: (Int, Int) -> ((Int, Int), ParentV) -> [(Int, Int)]
reconstructPath start (end, parentv) = reverse . takeWhile (/= start) . iterate (parentv !) $ end

main = do
    (start, end, grid) <- parseInput <$> readFile "input"
    let getPathLength s = length . reconstructPath s
    print $ getPathLength start $ bfs (==end) [start] (\a b -> b <= a + 1) grid (singleton start start)
    print $ getPathLength end   $ bfs ((== 0) . (grid !)) [end] (\a b -> b >= a - 1) grid (singleton end end)
