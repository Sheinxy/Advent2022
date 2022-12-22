module Main where

import Data.Char (isDigit)
import Data.Function (on)
import Data.List (groupBy)
import Data.Map  (Map, (!), fromList, member, notMember, findMin, findMax, filterWithKey, filter)

data Move = Step Int | Rotate String deriving (Show, Eq)

type Direction = Int
type Grid      = Map (Int, Int) Char
type Player    = (Int, Int, Direction)
type Wrapper   = (Grid -> Player -> (Player, Char))

parseInput :: [String] -> (Grid, [Move])
parseInput lines = (parseGrid . init $ init lines, parseMoves $ last lines)
    where parseMoves     = map parseMove . groupBy (on (==) isDigit)
          parseGrid grid = fromList $ concat [[((y, x), c) | (x, c) <- zip [1 .. ] line, c /= ' '] | (y, line) <- zip [1 .. ] grid]
          parseMove move | all isDigit move = Step (read move)
                         | otherwise        = Rotate move

[right, down, left, up] = [0 .. 3]

transitions :: Map (Int, Int, Direction) (Int, Int, Direction)
transitions = fromList $ concat [_14, _16, _23, _25, _26, _32, _34, _41, _43, _52, _56, _61, _62, _65]
    where _14 = [((  r, 50,   left), (151 - r,       1, right)) | r <- [  1 ..  50]]
          _16 = [((  0,  c,     up), (100 + c,       1, right)) | c <- [ 51 .. 100]]
          _23 = [(( 51,  c,   down), ( c - 50,     100,  left)) | c <- [101 .. 150]]
          _25 = [((  r, 151, right), (151 - r,     100,  left)) | r <- [  1 ..  50]]
          _26 = [((  0,   c,    up), (    200, c - 100,    up)) | c <- [101 .. 150]]
          _32 = [((  r, 101, right), (     50, r +  50,    up)) | r <- [ 51 .. 100]]
          _34 = [((  r,  50,  left), (    101, r -  50,  down)) | r <- [ 51 .. 100]]
          _41 = [((  r,   0,  left), (151 - r,      51, right)) | r <- [101 .. 150]]
          _43 = [((100,   c,    up), ( 50 + c,      51, right)) | c <- [  1 ..  50]]
          _52 = [((  r, 101, right), (151 - r,     150,  left)) | r <- [101 .. 150]]
          _56 = [((151,   c,  down), (100 + c,      50,  left)) | c <- [ 51 .. 100]]
          _61 = [((  r,   0,  left), (      1, r - 100,  down)) | r <- [151 .. 200]]
          _62 = [((201,   c,  down), (      1, c + 100,  down)) | c <- [  1 ..  50]]
          _65 = [((  r,  51, right), (    150, r - 100,    up)) | r <- [151 .. 200]]

simpleWrap :: Wrapper
simpleWrap grid (r, c, d) = ((r', c', d), t)
    where row             = filterWithKey (\(row, _) _ -> row == r) grid
          col             = filterWithKey (\(_, col) _ -> col == c) grid
          ((r', c'), t)   =    if d == up     then findMax col
                          else if d == right  then findMin row
                          else if d == down   then findMin col
                          else                     findMax row

cubicWrap :: Wrapper
cubicWrap grid (r, c, d) = ((r', c', d'), grid ! (r', c'))
    where (r', c', d') = transitions ! (r, c, d)

findNext :: Wrapper -> Grid -> Player -> (Player, Char)
findNext wrapper grid (r, c, d) | (r', c') `member` grid = ((r', c', d), grid ! (r', c'))
                                | otherwise              = wrapper grid (r', c', d)
                                where (dr, dc) = [(0, 1), (1, 0), (0, -1), (-1, 0)] !! d
                                      (r', c') = (r + dr, c + dc)

move :: Wrapper -> Grid -> Player -> Move -> Player
move wrap grid (r, c, d) (Rotate "R") = (r, c, [0 .. 3] !! ((d + 1) `mod` 4))
move wrap grid (r, c, d) (Rotate "L") = (r, c, [0 .. 3] !! ((d - 1) `mod` 4))
move wrap grid (r, c, d) (Step    s ) = go (r, c, d) s
                    where go player 0 = player
                          go player n = let (player', t) = wrap grid player in
                                        if t == '#' then player else go player' (n - 1)

main = do
    (grid, input) <- parseInput . lines <$> readFile "input"
    let (((r, c), _), d) = (findMin $ Data.Map.filter (/= '#') grid, right)
    let (r1, c1, d1)     = foldl (move (findNext simpleWrap) grid) (r, c, d) input
    let (r2, c2, d2)     = foldl (move (findNext  cubicWrap) grid) (r, c, d) input
    print (1000 * r1 + 4 * c1 + d1)
    print (1000 * r2 + 4 * c2 + d2)
