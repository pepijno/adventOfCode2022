module Main where

import Lib

calculateScore :: String -> Int
calculateScore "A X" = 4
calculateScore "A Y" = 8
calculateScore "A Z" = 3
calculateScore "B X" = 1
calculateScore "B Y" = 5
calculateScore "B Z" = 9
calculateScore "C X" = 7
calculateScore "C Y" = 2
calculateScore "C Z" = 6
calculateScore _ = 0

solve1 :: [String] -> Int
solve1 = sum . map calculateScore

calculateScore' :: String -> Int
calculateScore' "A X" = 3
calculateScore' "A Y" = 4
calculateScore' "A Z" = 8
calculateScore' "B X" = 1
calculateScore' "B Y" = 5
calculateScore' "B Z" = 9
calculateScore' "C X" = 2
calculateScore' "C Y" = 6
calculateScore' "C Z" = 7
calculateScore' _ = 0

solve2 :: [String] -> Int
solve2 = sum . map calculateScore'

main :: IO()
main = mainWrapper "day2" solve1 solve2
