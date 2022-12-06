module Main where

import Lib
import Data.List

isStartOfPacket :: (Eq a) => Int -> [a] -> Bool
isStartOfPacket n xs = (n==) $ length $ nub $ take n xs

countUntilStartOfPacket :: (Eq a) => Int -> [a] -> Int
countUntilStartOfPacket n ls@(a:xs)
  | isStartOfPacket n ls = n
  | otherwise = 1 + countUntilStartOfPacket n xs

solve1 :: [String] -> Int
solve1 = countUntilStartOfPacket 4 . head

solve2 :: [String] -> Int
solve2 = countUntilStartOfPacket 14 . head

main :: IO()
main = mainWrapper "day6" solve1 solve2
