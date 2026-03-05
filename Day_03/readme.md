## Day 03
### 🎒

Well, here's an easy one! Just finding a common item between some lists!

```hs
module Main where

import Data.Char
import Data.List

getPriority :: Char -> Int
getPriority c
    | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
    | otherwise            = ord c - ord 'A' + 27

splitMiddle :: [a] -> ([a], [a])
splitMiddle l = splitAt (length l `div` 2) l

chunk :: Int -> [a] -> [[a]]
chunk n = takeWhile (not . null) . map (take n) . iterate (drop n)

priorityOfCommonItem :: (String, String) -> Int
priorityOfCommonItem = getPriority . head . uncurry intersect

priorityOfBadge :: [String] -> Int
priorityOfBadge = getPriority . head . foldl1 intersect

main = do
    input <- lines <$> readFile "input"
    print $ sum $ map (priorityOfCommonItem . splitMiddle) input
    print $ sum $ map priorityOfBadge $ chunk 3 input
```

I don't think I really need to explain the first two functions, they're quite self-explanatory
in my opinion.

So let's first dive into the most obscure one: chunk!
```hs
chunk :: Int -> [a] -> [[a]]
chunk n = takeWhile (not . null) . map (take n) . iterate (drop n)
```
Well, I have a confession to make, I didn't write that one 😿
I took it from the [Haskell Wiki](https://wiki.haskell.org/Data.List.Split)
However I can still explain it!

First of all, what it does is that it splits a list into chunks of size n.
For example chunk 3 [1, 2, 3, 4, 5, 6] == [[1, 2, 3], [4, 5, 6]].

The way it does that is by iterating the list by dropping n elements each time.
So the first iteration is [1, 2, 3, 4, 5, 6], the second is [4, 5, 6] and the next ones are [].

Then for each iteration it takes the first n elements, so the first iteration will give [1, 2, 3], the second [4, 5, 6] and the next ones [].

Then finally, we take all these until we find an empty list, giving us [[1, 2, 3], [4, 5, 6]], because the first iteration gives [1, 2, 3], the second [4, 5, 6] and the third [].

Okay, now that this function has been explained, let's go onto the next one:
```hs
priorityOfCommonItem :: (String, String) -> Int
priorityOfCommonItem = getPriority . head . uncurry intersect
```
This takes a tuple containing two parts of a string (the two halves of the rucksack)
It is going to first compute the intersection (in the [mathematical sense](https://en.wikipedia.org/wiki/Intersection_%28set_theory%29)
of these two parts. Notice that the uncurry function here is to transform intersect :: [a] -> [a] -> [a] into intersect ([a], [a]) -> [a].
Then we just retrieve the first (and only) element of this intersection and we get its priority.

```hs
priorityOfBadge :: [String] -> Int
priorityOfBadge = getPriority . head . foldl1 intersect
```
This one is almost identical, except it doesn't take a tuple but a list of string. In fact, it is so identical that
I could have refactored my code to only have this function (by having splitMiddle returning a list instead of a couple for example), but whatever.
Here foldl1 is going to compute the intersection of the first two strings, then the intersection of that intersection and the third string (and so on and so forth if there were more strings, which isn't the case).

And finally the main function is nothing too fancy
```hs
main = do
    input <- lines <$> readFile "input"
    print $ sum $ map (priorityOfCommonItem . splitMiddle) input
    print $ sum $ map priorityOfBadge $ chunk 3 input
```
We get the lines of the input, for each line we split in the middle and get the priority of the common item, before summing all these priorities and printing it for part1,
and for part2 we do the same thing but instead of splitting each line in the middle we group them in groups of 3.
