module Main where

import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import Grid
import Lib

bfs :: Coord -> Grid Int -> Grid Int -> S.Set Coord -> [Coord] -> Grid Int
bfs _ _ dists _ [] = dists
bfs finish grid dists had (x : xs)
  | S.member x had = bfs finish grid dists had xs
  | otherwise = bfs finish grid newDists newHad (xs ++ legal)
  where
    ns = neighbours dir4 grid x
    height = grid M.! x
    currDist = dists M.! x
    legal = filter (\y -> (grid M.! y) >= height - 1) ns
    newDist n = if M.member n dists then min (1 + dists M.! x) (dists M.! n) else 1 + (dists M.! x)
    newDists = foldl (\ds y -> M.insert y (newDist y) ds) dists legal
    newHad = S.insert x had

parse :: [String] -> (Grid Int, (Coord, Coord))
parse xs = (grid', (start, finish))
  where
    grid = parseGrid ord xs
    grid' = M.map (\x -> if x == ord 'S' then ord 'a' else (if x == ord 'E' then ord 'z' else x)) grid
    start = head [(row, col) | (row, line) <- zip [0 ..] xs, (col, b) <- zip [0 ..] line, b == 'E']
    finish = head [(row, col) | (row, line) <- zip [0 ..] xs, (col, b) <- zip [0 ..] line, b == 'S']

solve1 :: [String] -> Int
solve1 xs = dists M.! finish
  where
    (grid, (start, finish)) = parse xs
    dists = bfs finish grid (M.singleton start 0) S.empty [start]

solve2 :: [String] -> Int
solve2 xs = minimum $ map (dists M.!) $ filter (`M.member` dists) as
  where
    (grid, (start, finish)) = parse xs
    dists = bfs finish grid (M.singleton start 0) S.empty [start]
    as = M.keys $ M.filter (== ord 'a') grid

main :: IO ()
main = mainWrapper "day12" solve1 solve2
