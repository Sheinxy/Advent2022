## Day 02
### 🪨📄✂️
It's time to code everyone's favorite game!

This is a pretty easy task to do, just throw a bunch of if else statements
and you're done, isn't this right?

```hs
module Main where

import Data.Char

parseInput :: String -> [(Int, Int)]
parseInput = map (\s -> (ord (s !! 0) - ord 'A', ord (s !! 2) - ord 'X')) . lines

playGame :: [Int] -> [Int] -> Int -> (Int, Int) -> Int
playGame ptsRes ptsAct cyc (p1, p2) = ptsAct !! p2 + cycle ptsRes !! (p1 + cyc * p2)

main = do
    input <- parseInput <$> readFile "input"
    print $ sum $ map (playgame [3, 0, 6] [1, 2, 3] 2) input
    print $ sum $ map (playgame [3, 1, 2] [0, 3, 6] 1) input
```

"WHERE ARE THE IF ELSE? YOU LIAR!"
![Crying Cat](https://www.meme-arsenal.com/memes/3dfa53fec8cd648be8b8f86dcc6ebf66.jpg)

Don't worry, I will explain everything in due time! Let's start first with the parsing
```hs
parseInput :: String -> [(Int, Int)]
parseInput = map (\s -> (ord (s !! 0) - ord 'A', ord (s !! 2) - ord 'X')) . lines
```
Pretty straightforward, just split the file into lines, and for each line get a couple
of two integers representing each player's move.

Okay, now that the easy part is done, let's tackle the really "what the heck" part
```hs
playGame :: [Int] -> [Int] -> Int -> (Int, Int) -> Int
playGame ptsRes ptsAct cyc (p1, p2) = ptsAct !! p2 + cycle ptsRes !! (p1 + cyc * p2)
```
"What the heck is going on here"
![Confused guy meme](https://cdn140.picsart.com/274517498014211.png?r1024x1024)

Well here is how everything works. Let us start with the easy part:

ptsAct is the list of points that are linked to the action I perform. That is
for part1 "If I play rock I get 1 point, paper 2 points and scissors 3 points",
and for part2 "If I lose I get 0 points, draw 3 points and win 6 points".
Hence this code in the main function:
```hs
    print $ sum $ map (playgame [3, 0, 6] [1, 2, 3] 2) input -- Notice the [1, 2, 3]
    print $ sum $ map (playgame [3, 1, 2] [0, 3, 6] 1) input -- Notice the [0, 3, 6]
```

ptsRes is a little bit harder to understand. It is the list of points that are linked
to how the action I perform is related to the action my opponent performs. That is
in part1 things likes "If I play rock and my opponent plays scissors, then I win so I get 6 points",
and for part2 "If I want to lose and my opponent plays rock then I play scissors and get 3 points".

Now the weird thing is "If this list is supposed to represent some kind of relation between
these two actions, then why is it a list of three values and not a matrix of 3x3?"

Glad you asked! ❤️  It all becomes clearer if we create said matrices:

|   	| 0 	| 1 	| 2 	|
|---	|---	|---	|---	|
| 0 	| 3 	| 6 	| 0 	|
| 1 	| 0 	| 3 	| 6 	|
| 2 	| 6 	| 0 	| 3 	|

|   	| 0 	| 1 	| 2 	|
|---	|---	|---	|---	|
| 0 	| 3 	| 1 	| 2 	|
| 1 	| 1 	| 2 	| 3 	|
| 2 	| 2 	| 3 	| 1 	|

Notice how each line/column is just the same shifted by some amount?

Well this offset is exactly was the cyc parameter is all about! (cyc for cycle you see!)
So basically, each time I ask for a specific column, I shift everything by that offset,
and because I'm using cycle I don't even need to use any modulo, and it works for both parts!

Then the main function is pretty straightforward: just parse the input, get the result for each game, sum the scores and print it!
