module Main where

import Data.Char
import Data.List
import qualified Data.Map as M
import Grid
import Lib

isBounded :: Coord -> Bool
isBounded (x, y) = x >= 0 && x < 99 && y >= 0 && y < 99

sightLine :: Grid Char -> Coord -> Dir4 -> [Char]
sightLine grid pos dir = [grid M.! x | x <- takeWhile isBounded $ tail $ iterate (`step4` dir) pos]

sightLines :: Grid Char -> Coord -> [[Char]]
sightLines grid pos = map (sightLine grid pos) [L, R, U, D]

isVisible :: Grid Char -> Coord -> Bool
isVisible grid pos = any (all (< h)) $ sightLines grid pos
  where
    h = grid M.! pos

solve1 :: [String] -> Int
solve1 xs = length $ filter (isVisible grid) [(x, y) | x <- [0 .. 98], y <- [0 .. 98]]
  where
    grid = parseGrid id xs

treesSeenDir :: (Ord a) => a -> [a] -> Int
treesSeenDir h xs = case break (>= h) xs of
  (a, []) -> length a
  (a, _ : _) -> length a + 1

treesSeen :: Grid Char -> Coord -> Int
treesSeen grid pos = product $ map (treesSeenDir h) $ sightLines grid pos
  where
    h = grid M.! pos

solve2 :: [String] -> Int
solve2 xs = maximum $ [treesSeen grid (x, y) | x <- [0 .. 98], y <- [0 .. 98]]
  where
    grid = parseGrid id xs

main :: IO ()
main = mainWrapper "day8" solve1 solve2
