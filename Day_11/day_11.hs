module Main where

import Data.List
import Data.List.Split

data Monkey = Monkey { items :: [Int], op :: (Int -> Int), test :: Int, throw :: (Int, Int), inspection :: Int}

parseInput :: String -> [Monkey]
parseInput = map (parseMonkey . tail . lines) . splitOn "\n\n"
    where parseMonkey [its, op, tst, t, f] = Monkey (parseItems its) (parseOp op) (readLast tst) (readLast t, readLast f) 0
          parseItems items = read $ "[" ++ (drop (length "  Starting items: ") items) ++ "]"
          parseOp = parseOperation . words . drop (length "  Operation: new = ")
          parseOperation [a, "*", b] = (\old -> (*) (if a == "old" then old else read a) (if b == "old" then old else read b))
          parseOperation [a, "+", b] = (\old -> (+) (if a == "old" then old else read a) (if b == "old" then old else read b))
          readLast = read . last . words

updateMonkey :: [Monkey] -> (Int, Int) -> [Monkey]
updateMonkey monkeys (i, item) = take i monkeys ++ [nmonkey] ++ drop (i + 1) monkeys
    where Monkey mitems mop mtest mthrow minspection = monkeys !! i
          common = product . map test $ monkeys
          nmonkey = Monkey (mitems ++ [item `mod` common]) mop mtest mthrow minspection

monkeyTurn :: Int -> [Monkey] -> Int -> [Monkey]
monkeyTurn rel monkeys i = take i nmonkeys ++ [nmonkey] ++ drop (i + 1) nmonkeys
    where Monkey mitems mop mtest (t, f) minspection = monkeys !! i
          nitems = map ((`div` rel) . mop) $ mitems
          nmonkeys = foldl updateMonkey monkeys [(if item `mod` mtest == 0 then t else f, item) | item <- nitems]
          nmonkey = Monkey [] mop mtest (t, f) (minspection + length mitems)

monkeyRound :: Int -> [Monkey] -> [Monkey]
monkeyRound rel monkeys = foldl (monkeyTurn rel) monkeys [0 .. length monkeys - 1]

main = do
    input <- parseInput <$> readFile "input"
    let play it rel = map inspection . (!! it) . iterate (monkeyRound rel)
    print $ product . take 2 . reverse . sort . play 20 3 $ input
    print $ product . take 2 . reverse . sort . play 10000 1 $ input
