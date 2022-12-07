## Day 06

ARE YOU READY FOR THE EASIEST DAY EVER?

For real, this one was so easy that it almost felt wrong.

One small anecdote about my life for this day: remember how I said that on day 4
I woke up really early and all? Well it inspired me to try waking up naturally, without any alarm clock,
and going to bed early by the same occasion. So here I was, at 9:30AM on a day without school.

Anyway, here is my solution:
```hs
module Main where

import Data.Function
import Data.List

firstDifferent :: Int -> [(Int, Char)] -> [(Int, Char)]
firstDifferent n =  head . dropWhile ((/= n) . length) .
                    map (nubBy (on (==) snd) . take n) . iterate tail

main = do
    input <- zip [1 .. ] <$> readFile "input"
    print $ fst . last $ firstDifferent 4 input
    print $ fst . last $ firstDifferent 14 input
```
Small eh?

```hs
firstDifferent :: Int -> [(Int, Char)] -> [(Int, Char)]
firstDifferent n =  head . dropWhile ((/= n) . length) .
                    map (nubBy (on (==) snd) . take n) . iterate tail
```
So to find the first n different characters, we first iterate through the string character by character.
That is, if our string is "helloworld", our first iteration is "helloworld", our second "elloworld", our third "lloworld" etc.

Then, for each iteration, we take the n first characters, and we remove any duplicates (so "lloworld" might become "low" if we take 4 characters)
We discard every iteration until we find one where removing duplicates doesn't change anything, and we take that one as our result.

That's all.
Really.

The only other thing that I'm doing is that I zip my input with [1 .. ] to get the index of each character, because then I only
need to know the index of the last character in our packet.

But that's really all there is to it.

![Cat with sunglasses because it's rad](https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fwallpaperaccess.com%2Ffull%2F621550.jpg&f=1&nofb=1&ipt=be36eabe17b7991d3ca1ae566b8ed97125008975dcc55654dc2831bd6b15ec26&ipo=images)
