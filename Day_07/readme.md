## Day 07
### 📁🌲

Finally, things start to get interesting! 😸

I wasn't ready to do this one at 7:30AM, I just wasn't ready.

Well, in retrospect, although at first I thought "how am I going to do that", this was actually quite easy.

```hs
module Main where

import Data.List

data File = Dir String [File] | File String Int deriving (Show)

getFilesystem :: [String] -> File -> ([String], File)
getFilesystem [] fs = ([], fs)
getFilesystem (cmd : cmds) (Dir name files)
    | cmd == "$ cd .."        = (cmds, Dir name files)
    | "dir"  `isPrefixOf` cmd = getFilesystem cmds (Dir name files)
    | "$ ls" `isPrefixOf` cmd = getFilesystem cmds (Dir name files)
    | "$ cd" `isPrefixOf` cmd = getFilesystem ncmds (Dir name (sfs : files))
    | otherwise = getFilesystem cmds (Dir name (nf : files))
    where
        [size, fname] = words cmd
        nf = File fname (read size)
        (ncmds, sfs) = getFilesystem cmds (Dir (drop 5 cmd) [])

getSize :: File -> Int
getSize (File _ size) = size
getSize (Dir _ files) = sum  . map getSize $ files

getDirSizes :: File -> [Int]
getDirSizes (File _ _)    = []
getDirSizes (Dir n files) =
    getSize (Dir n files) : foldl (\a d -> a ++ getDirSizes d) [] files

main = do
    input <- snd . flip getFilesystem (Dir "/" []) . drop 2 . lines
             <$> readFile "input"
    let dirSizes = getDirSizes input
    let unused = 70000000 - getSize input
    print $ sum . filter (<= 100000) $ dirSizes
    print $ minimum . filter (>= (30000000 - unused)) $ dirSizes
```

Oh my... Would you look at that, a datatype!
```hs
data File = Dir String [File] | File String Int deriving (Show)
```
So, this datatype represents a file, which is either a Directory which has a name and a bunch of files in it, or a regular file with a name and a size.

So this is just a general tree, Dir is an internal node and File is a leaf.

The hardest part here was parsing the input
```hs
getFilesystem :: [String] -> File -> ([String], File)
getFilesystem [] fs = ([], fs)
getFilesystem (cmd : cmds) (Dir name files)
    | cmd == "$ cd .."        = (cmds, Dir name files)
    | "dir"  `isPrefixOf` cmd = getFilesystem cmds (Dir name files)
    | "$ ls" `isPrefixOf` cmd = getFilesystem cmds (Dir name files)
    | "$ cd" `isPrefixOf` cmd = getFilesystem ncmds (Dir name (sfs : files))
    | otherwise = getFilesystem cmds (Dir name (nf : files))
    where
        [size, fname] = words cmd
        nf = File fname (read size)
        (ncmds, sfs) = getFilesystem cmds (Dir (drop 5 cmd) [])
```
Thanks god recursion for existing!

Basically, all we need is a stack to push and pop current working directories, which recursion already provides!

When we do "cd ..", we pop from the stack, the old directory is added to the new one, and we go on with the rest of the commands.
```hs
    | cmd == "$ cd .."        = (cmds, Dir name files)
```
In fact when we do "cd ..", we just give back the rest of the commands and the directory we just finished building


When we do "cd [someDir]" on the other hand, we push a new directory onto the stack (the one we go into) by going into our recursive function. 
This recursive call is actually going to give us the rest of the commands and the old directory that we have to add into our directory
```hs
        (ncmds, sfs) = getFilesystem cmds (Dir (drop 5 cmd) [])
```
This line is responsible for recursively going into our subdirectory and retrieving it alongside the rest of the commands after our call
```hs
    | "$ cd" `isPrefixOf` cmd = getFilesystem ncmds (Dir name (sfs : files))
```
And this one is responsible for performing the next commands in our directory as well as adding our subdirectory into the files of our current one

And finally
```hs
    | "dir"  `isPrefixOf` cmd = getFilesystem cmds (Dir name files)
    | "$ ls" `isPrefixOf` cmd = getFilesystem cmds (Dir name files)
```
Basically we assume that the input is well-formed, that is: there is only ONE ls after each cd, and each cd goes into an existing dir.
So we don't care about the dir and ls lines!

That was for the hard part, not the easiest to understand, but it works.

```hs
getSize :: File -> Int
getSize (File _ size) = size
getSize (Dir _ files) = sum  . map getSize $ files
```
For regular files, the size is just... well the file size.

And for directories, it is the sum of the sizes of its subfiles and subdirectories.

Nothing too hard here to be honest, I believe it is self-explanatory.

```hs
getDirSizes :: File -> [Int]
getDirSizes (File _ _)    = []
getDirSizes (Dir n files) =
    getSize (Dir n files) : foldl (\a d -> a ++ getDirSizes d) [] files
```
Now THIS, however, IS INTERESTING. Remember how I built a whole file tree? Well turns out it was useless.
In fact I just needed the directories sizes for both part (and here I was, trying not to lose any information, when I could just take that)

As I didn't want to redo the parsing, I just made this function to get a list of directory sizes. It's just a depth-first traversal where for each directory
I go through its subfiles and get its directory list, concatenate the results, and add the size of the current directory as well.
For regular files there is not directory, so they can just return an empty list, this way when concatenated they amount to virtually nothing.

And finally

```hs
main = do
    input <- snd . flip getFilesystem (Dir "/" []) . drop 2 . lines
             <$> readFile "input"
    let dirSizes = getDirSizes input
    let unused = 70000000 - getSize input
    print $ sum . filter (<= 100000) $ dirSizes
    print $ minimum . filter (>= (30000000 - unused)) $ dirSizes
```
Once I have the list of directory sizes, it's all a matter of filtering, there is nothing quite hard in it, it is all too self-explanatory.


Well, I have a confidence to make: this is not my original solution.

Remember how I said that I didn't want to lose information, but then realized I only needed a list of sizes? Well because of that, my original solution basically traversed
the general tree to get the sum of small directories in part1, and to get the minimum in part2, which is not as pretty or self-explanatory.

Had I not tried to anticipate too much here by building a tree, this would have gone much faster 😿
