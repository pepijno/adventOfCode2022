module Main where

import Lib
import Parser
import Data.List

parseInput :: Parser ([Int], [Int])
parseInput = do
  n1 <- integer
  char '-'
  n2 <- integer
  char ','
  n3 <- integer
  char '-'
  n4 <- integer
  pure ([n1..n2], [n3..n4])

isContainedWithin :: (Eq a) => [a] -> [a] -> Bool
isContainedWithin l1 l2 = i == l1 || i ==l2
  where 
    i = intersect l1 l2

solve1 :: [String] -> Int
solve1 = length . filter (uncurry isContainedWithin . unsafeParse parseInput)

doesOverlap :: (Eq a) => [a] -> [a] -> Bool
doesOverlap l1 l2 = not $ null $ intersect l1 l2

solve2 :: [String] -> Int
solve2 = length . filter (uncurry doesOverlap . unsafeParse parseInput)

main :: IO()
main = mainWrapper "day4" solve1 solve2
