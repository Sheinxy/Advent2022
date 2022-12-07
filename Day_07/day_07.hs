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
