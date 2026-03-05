## Day 05
### 🏗️📦

Hey everyone, look! It's the puzzle where everyone was scared of parsing the input!

For real, how many of you here simply hardcoded the input? 👀

I, for one, sure didn't!
```hs
module Main where

import Data.Char
import Data.List

parseInput :: String -> ([String], [[Int]])
parseInput = parseLines . span (not . null) . lines
    where
        parseStacks = map head . takeWhile (not . null) . iterate (drop 4) . tail .
                      map (dropWhile (== ' ')) . transpose . init
        parseActions = map (map read . filter (all isDigit) . words) . tail
        parseLines (stk, act) = (parseStacks stk, parseActions act)

moveStacks :: Bool -> [String] -> [Int] -> [String]
moveStacks rev stacks [n, src, dst] = [
        if i == src then drop n s
        else if i == dst then getFromStack n src ++ s
        else s | (i, s) <- zip [1 .. ] stacks
    ]
    where getFromStack n src
            | rev = reverse . take n $ stacks !! (src - 1)
            | otherwise = take n $ stacks !! (src - 1)

main = do
    input <- parseInput <$> readFile "input"
    print $ map head . filter (not . null) $ uncurry (foldl $ moveStacks True) input
    print $ map head . filter (not . null) $ uncurry (foldl $ moveStacks False) input
```

Ok there is A WHOLE LOT to unpack here, let's start with the hardest part
```hs
parseInput :: String -> ([String], [[Int]])
parseInput = parseLines . span (not . null) . lines
    where
        parseStacks = map head . takeWhile (not . null) . iterate (drop 4) . tail .
                      map (dropWhile (== ' ')) . transpose . init
        parseActions = map (map read . filter (all isDigit) . words) . tail
        parseLines (stk, act) = (parseStacks stk, parseActions act)
```
This parses the input, returning a list of stacks and a list of moves.

```hs
parseInput = parseLines . span (not . null) . lines
```
The first thing is splitting the input into lines, and then splitting these line into two parts.
These two parts being separated by and empty line, the first part is the elements spanning non-empty lines
(basically, all the first elements before the first empty line), and the second part is everything else after.

Then, once we've got these two parts, corresponding to the stacks and the actions, we need to parse them.

Parsing the actions is pretty easy
```hs
        parseActions = map (map read . filter (all isDigit) . words) . tail
```
First we get rid of the first empty line (it's a leftover of span). Then for each
action, we split the action into words, we only keep IN ORDER the ones corresponding to numbers (they're the interesting ones, and they're always in the same order)
and we read them into integers.

Then there is the weird part: how to parse this:
```
    [P]                 [C] [C]
    [W]         [B]     [G] [V] [V]
    [V]         [T] [Z] [J] [T] [S]
    [D] [L]     [Q] [F] [Z] [W] [R]
    [C] [N] [R] [H] [L] [Q] [F] [G]
[F] [M] [Z] [H] [G] [W] [L] [R] [H]
[R] [H] [M] [C] [P] [C] [V] [N] [W]
[W] [T] [P] [J] [C] [G] [W] [P] [J]
 1   2   3   4   5   6   7   8   9
```

Like so 😸:
```hs
        parseStacks = map head . takeWhile (not . null) . iterate (drop 4) . tail .
                      map (dropWhile (== ' ')) . transpose . init
```
Well first of all, I don't care about the last line, so I discard it with init.
Then I transpose everything: every column becomes a line, which means for example that my sixth line is now "PWVDCMHT",
WHICH IS EXACTLY WHAT I WANT FOR THE SECOND STACK!!! 🙀

Sadly, not all lines are as great: the second line for example is made of a bunch of ' ' followed by "FRW", and we only want that last part,
so for each line we drop every leading whitespace (there is no trailing whitespace so no worries there).

Then there is last one small problem to handle, notice that, before we even transposed everything, we only really care about every fifth column (starting from the second one)
Well now we need to only keep every fifth line because we tranposed, but it doesn't change anything to how I did it:
 - First, discard the first column, so we start with the second column, using tail
 - Next iterate by droping 4 columns each time, that is useful because the head element of the first iteration is going to be the first column, then the fifth, then the ninth etc.
 - Iterate until there are no columns left
 - Take the head of each iteration (recall that they were the first, fifth, ninth etc. which are the ones we want to keep!)
And that gives us our list of stacks! 😸

Now, what do we do with these stacks? Well we move them obviously!
```hs
moveStacks :: Bool -> [String] -> [Int] -> [String]
moveStacks rev stacks [n, src, dst] = [
        if i == src then drop n s
        else if i == dst then getFromStack n src ++ s
        else s | (i, s) <- zip [1 .. ] stacks
    ]
    where getFromStack n src
            | rev = reverse . take n $ stacks !! (src - 1)
            | otherwise = take n $ stacks !! (src - 1)
```
Of course, this being haskell, we can't simply do something like stacks[dst] = src[:n], that would be too simple.
So what I am doing instead is that I go through every element of the stack, zipped with an index (so I get (1, "FRW") for the first stack for example),
and if that index is my source index I drop n elements, if it is the destination index I concat n elements from the source stack to my stack, otherwise I just take my stack.
And to get the n elements from the source stack, I just may need to reverse depending on whether we're using the crane9000 or crane9001, (basically in crane9000 we don't really move
the elements all at once, but one by one, which is akin to simply reversing when we take)

And that leaves us with the main function
```hs
main = do
    input <- parseInput <$> readFile "input"
    print $ map head . filter (not . null) $ uncurry (foldl $ moveStacks True) input
    print $ map head . filter (not . null) $ uncurry (foldl $ moveStacks False) input
```
The two parts are identical in how they work, we simply don't reverse when moving in the second one.
First we perform all the moves onto our stack and we get our final stack, then we discard any possibly empty stack, and finally we just read the first character of each stack.

That is actually really simple!
