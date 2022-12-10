module Main where

parseInput :: String -> [Int]
parseInput = concatMap (\l -> 0 : if l == "noop" then [] else [read . last . words $ l]) . lines

chunk :: Int -> [a] -> [[a]]
chunk n = takeWhile (not . null) . map (take n) . iterate (drop n)

drawLine :: [Int] -> IO()
drawLine = putStrLn . map getChar . zip [0 .. ]
    where getChar (c, x) = if abs (c - x) <= 1 then '#' else '.'

main = do
    input <- parseInput <$> readFile "input"
    let cycles = init . scanl (+) 1 $ input
    print $ sum [cycles !! (i - 1) * i | i <- [20, 60 .. 220]] 
    mapM drawLine . chunk 40 $ cycles
