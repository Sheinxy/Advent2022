module Main where

import Data.List (transpose)
import Data.Set (Set, fromList, findMin, findMax, notMember, member, insert, singleton)
import qualified Data.Map as M (Map, notMember, fromList, (!))

data World = World { grid :: Set (Int, Int), cycles :: M.Map Int (Set (Int, Int)), height :: Int, width :: Int} deriving (Show, Eq)

parseInput :: String -> World
parseInput input     = World grid cycles h w
    where dirs       = M.fromList [('<', (0, -1)), ('>', (0, 1)), ('v', (1, 0)), ('^', (-1, 0))]
          (ls, cs)   = (lines input, transpose ls)
          (w, h)     = (length cs - 2, length ls - 2)
          grid       = fromList [(r, c) | (r, l) <- zip [-1 .. ] ls, (c, t) <- zip [-1 .. ] l, t /= '#']
          blizzards  = [((r, c), dirs M.! t)  | (r, l) <- zip [-1 .. ] ls, (c, t) <- zip [-1 .. ] l, t `elem` "<>v^"]
          cycles     = M.fromList [(cyc, fromList [((r + dr * cyc) `mod` h, (c + dc * cyc) `mod` w) | ((r, c), (dr, dc)) <- blizzards]) | cyc <- [0 .. lcm w h]]

bfs :: World -> Set ((Int, Int), Int) -> [((Int, Int), Int)] -> (Int, Int) -> Int
bfs world seen (((r, c), t):xs) end | end == (r, c) = t
                                    | found         = t'
                                    | otherwise     = bfs world seen' queue' end
                        where (w, h)                = (width world, height world)
                              (t', cyc)             = (t + 1, t' `mod` (lcm w h))
                              (gr, blizz)           = (grid world, cycles world M.! cyc)
                              accessible p          = (p, cyc) `notMember` seen && p `member` gr && p `notMember` blizz
                              neighbours            = filter accessible [(r + 1, c), (r, c + 1), (r - 1, c), (r, c - 1), (r, c)]
                              found                 = any (== end) neighbours
                              seen'                 = foldr (\p -> insert (p, cyc)) seen neighbours
                              queue'                = xs ++ map (\p -> (p, t')) neighbours

main = do
    input <- parseInput <$> readFile "input"
    let (start, end) = (findMin $ grid input, findMax $ grid input)
    let common       = lcm (width input) (height input)
    let t1           = bfs input (singleton (start,               0)) [(start,  0)] end
    let t2           = bfs input (singleton (  end, t1 `mod` common)) [(end ,  t1)] start
    let t3           = bfs input (singleton (start, t2 `mod` common)) [(start, t2)] end
    print t1
    print t3
