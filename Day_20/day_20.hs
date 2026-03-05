module Main where

import Data.List

parseInput :: String -> [(Int, Int)]
parseInput = zip [0 .. ] . map read . lines

moveAround :: Int -> [(Int, Int)] -> Int -> [(Int, Int)]
moveAround len num n = n'
    where (i, j) = num !! n
          i' = (i + j) `mod` (len - 1)
          i'' = if i' == 0 then len - 1 else i'
          newPos k = if k == i then i'' else if k >= i'' && k < i then k + 1 else if k <= i'' && k > i then k - 1 else k
          n' = [(newPos k, l) | (k, l) <- num]

solve :: [(Int, Int)] -> Int -> Int
solve input iter = sum [snd $ grove !! ((zeroi + i) `mod` len) | i <- [1000, 2000, 3000]]
    where len = length input
          grove = sort . (!! iter) . iterate (flip (foldl (moveAround len)) [0 .. len - 1]) $ input
          zeroi = fst . head . filter ((== 0) . snd) $ grove

main = do
    input <- parseInput <$> readFile "input"
    let decrypted = map (\(a, b) -> (a, b * 811589153)) input
    print $ solve input 1
    print $ solve decrypted 10

