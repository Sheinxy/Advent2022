## Day 01

New year, same me! Once again, I am doing this in Haskell. At first I wanted
to do it using PICO-8, so that I could create neat visualisations, but I
couldn't be bothered with learning Lua, plus some of my friends wanted to try
doing it in Haskell as well, so here I am.

For once, I wanted to give a detailed explanation of what I do, in a blog-like
format (not sure if this format is the greatest, I might want to create a whole
separated repo for blogs, I'll see)

So, let's head into the heart of the matter now, shall we? Day 1! As expected
this first puzzle is fairly straightforward, so here is my code:

```hs
module Main where

import Data.List
import Data.List.Split

parseInput :: String -> [Int]
parseInput =  map (sum . map read . lines) . splitOn "\n\n"

main = do
    input <- reverse . sort . parseInput <$> readFile "input"
    print $ head input
    print $ sum $ take 3 input
```

Pretty neat eh? I love Haskell so much <3

But I know what you're here for, explanations! Well here they are:

First of all, let's start with the main function:
```hs
main = do
    input <- reverse . sort . parseInput <$> readFile "input"
    print $ head input
    print $ sum $ take 3 input
```
Four lines, the first one reads the input file and parses it (it gets the total
amount of calories for each elf), and then sorts it in decreasing order (or rather
it first sorts it in increasing order and then reverse it), then we just print
the first element of the list (therefore the maximum), and then the sum of the 3
first elements. Nothing to fancy here, pretty straightforward!

But now, let's dive into the really interesting part!
```hs
parseInput :: String -> [Int]
parseInput =  map (sum . map read . lines) . splitOn "\n\n"
```
This function takes a String, which is the whole input file, and gives back a list
of integers, each integer being the number of calories carried by an elf.

But "how does it work?", I hear you ask! Well first of all we split the string
whenever we see two newlines following each other (basically we split into chunks
of calories corresponding to each elf), then for each chunk we first split into
lines, and for each line we read the string into an int, and then we sum all the lines
together to get the total amount of calories. And voilà! Nothing more, nothing less,
as I said, it's the first day so it's pretty easy <3
