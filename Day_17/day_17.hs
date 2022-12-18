module Main where

import Data.Set (Set, member, insert, empty, foldl', toList, difference, filter)
import qualified Data.Map as M (Map, (!), insert, member, empty)

type Block = [(Int, Int)]

getHeight :: Set (Int, Int) -> Int
getHeight tower = fst (foldl' max (0, 0) tower)

getTopFourRow :: Set (Int, Int) -> [(Int, Int)]
getTopFourRow tower = map (\(r, c) -> (r - height, c)) . toList . Data.Set.filter ((>= height - 4) . fst) $ tower
    where height    = getHeight tower

spawn :: Set (Int, Int) -> Block -> Block
spawn tower block = [(r + rmin, c + 2) | (r, c) <- block]
    where rmin    = 4 + getHeight tower

push :: Set (Int, Int) -> Block -> Int -> Block
push tower block dir | inBound && notBlock   = block'
                     | otherwise             = block
    where block'   = [(r, c + dir) | (r, c) <- block]
          inBound  = all (\(_, c) -> c >= 0 && c < 7) block'
          notBlock = not $ any (`member` tower) block'

fall :: Set (Int, Int) -> Block -> Block
fall tower block     | inBound && notBlock   = block'
                     | otherwise             = block
    where block'   = [(r - 1, c) | (r, c) <- block]
          inBound  = all ((> 0) . fst) block'
          notBlock = not $ any (`member` tower) block'

land :: Set (Int, Int) -> Block -> [Int] -> (Block, [Int])
land tower block dirs | hasStopped =      (block', tail dirs)
                      | otherwise  = land tower block' (tail dirs)
    where pushed      = push tower block (head dirs)
          block'      = fall tower pushed
          hasStopped  = block' == pushed

play :: (Set (Int, Int), [Block], [Int]) -> (Set (Int, Int), [Block], [Int])
play (tower, incoming, dirs) = (tower', tail incoming, dirs')
    where newBlock           = spawn tower (head incoming)
          (block', dirs')    = land tower newBlock dirs
          tower'             = foldl (flip insert) tower block'

findCycle :: [Block] -> [Int] -> ([Int], Int, Int)
findCycle blocks jets = go M.empty [] 0 (empty, blocks, cycle jets)
    where jetLen = length jets
          go memo heights i (tower, incoming, dirs) | status `M.member` memo = let s = memo M.!status in (reverse $ (getHeight tower):heights, i, s)
                                                    | otherwise = go (M.insert status i memo) ((getHeight tower):heights) (i + 1) $ play (tower, incoming, dirs)
                                                  where status = (getTopFourRow tower, head incoming, take jetLen dirs)

solve :: Int -> [Int] -> [Block] -> Int
solve n jets blocks = if n < start then heights !! n else q * cycleHeight + heights !! (start + m)
    where (heights, end, start) = findCycle blocks jets
          period                = end - start
          after                 = n - start
          cycleHeight           = last heights - heights !! start
          (q, m)                = (after `div` period, after `mod` period)

main = do
    input <- map (\c -> if c == '>' then 1 else -1) . init <$> readFile "input"
    let blocks = cycle [[(0, 0), (0, 1), (0, 2), (0, 3)], 
                        [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)], 
                        [(0, 0), (0, 1), (0, 2), (1, 2), (2, 2)],
                        [(0, 0), (1, 0), (2, 0), (3, 0)],
                        [(0, 0), (0, 1), (1, 0), (1, 1)]]
    print $ solve 2022 input blocks
    print $ solve 1000000000000 input blocks 
