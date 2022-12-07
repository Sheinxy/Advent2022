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

sumLess :: [File] -> Int -> Int
sumLess [] acc = acc
sumLess (x : xs) acc
    | File _ _    <- x = sumLess xs acc
    | Dir _ files <- x = sumLess xs newAcc + sumLess files 0
    where size = getSize x
          newAcc = acc + if size > 100000 then 0 else size

findSmallest :: Int -> Int -> [File] -> Int
findSmallest _ smallest [] = smallest
findSmallest toFree smallest (File _ _ : xs) = findSmallest toFree smallest xs
findSmallest toFree smallest (Dir n files : xs) = findSmallest toFree nsmall xs
    where nsmall = minimum $ filter (>= toFree)
                   [
                    smallest, 
                    getSize (Dir n files), 
                    findSmallest toFree smallest files
                   ]

main = do
    (Dir _ files) <- snd . flip getFilesystem (Dir "/" []) . drop 2 . lines 
            <$> readFile "input"
    print $ sumLess files 0
    let rootSize = getSize (Dir "/" files)
    let toFree = 30000000 - (70000000 - rootSize)
    print $ findSmallest toFree rootSize files
