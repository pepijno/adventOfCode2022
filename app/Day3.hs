module Main where

import Lib
import Data.List
import Data.Char

charToInt :: Char -> Int
charToInt c = if c < 'a' then (ord c) - 38 else (ord c) - 96

splitRugsack :: [a] -> ([a], [a])
splitRugsack xs = (take l xs, drop l xs)
  where
    l = (length xs) `div` 2

findDuplicate :: (Eq a) => ([a], [a]) -> a
findDuplicate (a, b) = head $ intersect a b

findDup :: (Eq a) => [[a]] -> a
findDup [a, b, c] = head $ intersect a $ intersect b c

split :: Int -> [a] -> [[a]]
split n [] = []
split n xs = (take n xs):(split n (drop n xs))

solve1 :: [String] -> Int
solve1 = sum . map (charToInt . findDuplicate . splitRugsack)

solve2 :: [String] -> Int
solve2 = sum . map (charToInt . findDup) . split 3

main :: IO()
main = mainWrapper "day3" solve1 solve2
