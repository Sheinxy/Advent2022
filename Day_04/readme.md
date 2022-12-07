## Day 04

Oh do I love it when my solution is way different from my friends' solutions!

One fun fact about that one, I did it at about 7:30AM, because I couldn't sleep
(I got about 4 hours of sleep that night, which seldom happens to me), so I decided
to get up, and I solved it while eating breakfast.

So, without further ado, here is my solution:
```hs
module Main where

import Data.List
import Data.List.Split

parseInput :: String -> [[[Int]]]
parseInput = map (map parsePair . splitOn ",") . lines
    where
        interval [a, b] = [read a .. read b]
        parsePair = interval . splitOn "-"

fullyContains :: [[Int]] -> Bool
fullyContains l = foldl1 intersect l `elem` l

overlaps :: [[Int]] -> Bool
overlaps = not . null . foldl1 intersect

main = do
    input <- parseInput <$> readFile "input"
    print $ length . filter (== True) $ map fullyContains input
    print $ length . filter (== True) $ map overlaps input
```

The "hardest" part for me here was to parse the input (and by hardest I mean the one that took more than 30 seconds)
```hs
parseInput :: String -> [[[Int]]]
parseInput = map (map parsePair . splitOn ",") . lines
    where
        interval [a, b] = [read a .. read b]
        parsePair = interval . splitOn "-"
```
Basically I get every line of the input, and for each line I split into two parts separated by a comma, 
and for each part I then split into two more parts separated by a dash, each of these two parts representing
a bound of an interval that I then proceed to create. Therefore, if I have "1-3,4-5\n2-4,3-5",
my function gives me [[[1, 2, 3], [4, 5]], [[2, 3, 4], [3, 4, 5]]].

Then
```hs
fullyContains :: [[Int]] -> Bool
fullyContains l = foldl1 intersect l `elem` l

overlaps :: [[Int]] -> Bool
overlaps = not . null . foldl1 intersect
```
These two are mostly self explanatory.

The first one checks if one of the list is a subset of the other, so we simply compute the intersection
of the list, and by a wonderful property we know that if $A \cap B = A \Rightarrow A \subset B$, so we only
need to check if this intersection is inside our list of intervals.

The second one is even more self explanatory. We just check if the intersection is not null, nothing more than that.
I really don't know what more to say about it to be honest, it's just basic math

And finally the main function is also self explanatory
```hs
main = do
    input <- parseInput <$> readFile "input"
    print $ length . filter (== True) $ map fullyContains input
    print $ length . filter (== True) $ map overlaps input
```
For each pair of intervals we check if one is a subset of the other/if they overlap
and we count the number of which satisfy this property.

I really don't know what to say, it's all just basic maths, and I didn't even needed to
check bounds.
