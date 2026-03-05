module Main where

import Data.Set (Set, member, notMember, delete, fromList, findMin, findMax, union, foldl)
import qualified Data.Set as S (map)

parseInput :: String -> Set (Int, Int, Int)
parseInput = fromList . map read . map ("(" ++) . map (++ ")") . lines

getNeighbours :: Set (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int, Int)]
getNeighbours world (x, y, z) = filter (`member` world) [(x - 1, y, z), (x + 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1)]

getSurface :: Set (Int, Int, Int) -> Int
getSurface world = foldl (flip $ (+) .  (6 -) .  length . getNeighbours world) 0 world

getNegativeSpace :: Set (Int, Int, Int) -> Set (Int, Int, Int)
getNegativeSpace world = fromList [(x, y, z) | x <- [minX .. maxX], y <- [minY .. maxY], z <- [minZ .. maxZ], (x, y, z) `notMember` world]
    where xs = S.map (\(x, _, _) -> x) world
          ys = S.map (\(_, y, _) -> y) world
          zs = S.map (\(_, _, z) -> z) world
          (minX, minY, minZ) = (findMin xs - 1, findMin ys - 1, findMin zs - 1)
          (maxX, maxY, maxZ) = (findMax xs + 1, findMax ys + 1, findMax zs + 1)

getInside :: Set (Int, Int, Int) -> [(Int, Int, Int)] -> Set (Int, Int, Int)
getInside negative [] = negative
getInside negative (el:queue) = getInside negative' queue'
    where neighbours = getNeighbours negative el
          negative'  = foldl (flip delete) negative neighbours
          queue'     = queue ++ neighbours
          
main = do
    input <- parseInput <$> readFile "input"
    let negative = getNegativeSpace input
    let start = findMin negative
    let inside = getInside (delete start negative) [start]
    let lavaDrop = input `union` inside
    print $ getSurface input
    print $ getSurface lavaDrop
