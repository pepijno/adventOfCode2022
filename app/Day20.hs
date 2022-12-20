module Main where

import Lib
import qualified Data.Vector as V

parseInput :: [String] -> [Int]
parseInput = map read

insertAt :: Int -> (Int, Int) -> V.Vector (Int, Int) -> V.Vector (Int, Int)
insertAt i x xs = before V.++ V.cons x after
  where
    (before, after) = V.splitAt i xs

unmix :: Int -> V.Vector (Int, Int) -> V.Vector (Int, Int) -> V.Vector (Int, Int)
unmix i list newList
  | V.null list = newList
  | otherwise = unmix i list' unmixedList
  where
    Just index = V.findIndex ((==y) . fst) newList
    to = (x + index) `mod` i
    unmixedList = insertAt to p (b V.++ V.tail a)
    (b, a) = V.splitAt index newList
    p@(y, x) = V.head list
    list' = V.tail list

unmixMultipleTimes :: Int -> V.Vector (Int, Int) -> V.Vector (Int, Int) -> V.Vector (Int, Int)
unmixMultipleTimes 0 _ list' = list'
unmixMultipleTimes n list list' = unmixMultipleTimes (n - 1) list (unmix (V.length list - 1) list list')

findSum :: V.Vector (Int, Int) -> Int
findSum list = sum $ fmap (\x -> snd (list V.! x)) indices
  where
    l = V.length list
    Just i = V.findIndex ((==0) . snd) list
    indices = map (\x -> (x + i) `mod` l) [1000, 2000, 3000]

solve1 :: [String] -> Int
solve1 xs = findSum $ unmixMultipleTimes 1 vec vec
  where
    vec = V.indexed . V.fromList . parseInput $ xs

solve2 :: [String] -> Int
solve2 xs = findSum $ unmixMultipleTimes 10 vec vec
  where
    vec = V.indexed . V.fromList . map (*811589153) . parseInput $ xs

main :: IO()
main = mainWrapper "day20" solve1 solve2
