module Main where

import Data.List
import Lib

solve1 :: [String] -> Int
solve1 = maximum . map (sum . map read) . groupPairs

solve2 :: [String] -> Int
solve2 = sum . take 3 . reverse . sort . map (sum . map read) . groupPairs

main :: IO ()
main = mainWrapper "day1" solve1 solve2
