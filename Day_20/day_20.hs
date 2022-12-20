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

main = do
    input <- parseInput <$> readFile "input"
    let len = length input
    let grove = sort $ foldl (moveAround len) input [0 .. len - 1]
    let zeroi = fst . head . filter ((== 0) . snd) $ grove
    print $ sum [snd (grove !! ((zeroi + i) `mod` len)) | i <- [1000, 2000, 3000]]
    let decrypted = map (\(a, b) -> (a, b * 811589153)) input
    let grove = sort . (!! 10) . iterate (flip (foldl (moveAround len)) [0 .. len - 1]) $ decrypted
    let zeroi = fst . head . filter ((== 0) . snd) $ grove
    print $ sum [snd (grove !! ((zeroi + i) `mod` len)) | i <- [1000, 2000, 3000]]

