module Main where

snafuToDec :: String -> Int
snafuToDec = foldl (\acc digit -> 5 * acc + digitToDec digit) 0
    where digitToDec '0' =  0
          digitToDec '1' =  1
          digitToDec '2' =  2
          digitToDec '-' = -1
          digitToDec '=' = -2

decToSnafu :: Int -> String
decToSnafu 0 = ""
decToSnafu n = decToSnafu n' ++ units !! rem
    where units = ["0", "1", "2", "=", "-"]
          rem   = n `mod` 5
          n'    = n `div` 5 + if rem <= 2 then 0 else 1

parseInput :: String -> [Int]
parseInput = map snafuToDec . lines

main = do
    input <- parseInput <$> readFile "input"
    print $ decToSnafu . sum $ input
