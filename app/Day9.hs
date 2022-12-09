module Main where

import Lib
import Parser
import Grid
import Data.List
import qualified Data.Set as S

parseCommand :: Parser [Dir4]
parseCommand = do
  dir <- charToDir <$> anyChar
  char ' '
  amount <- natural
  return $ replicate amount dir

charToDir :: Char -> Dir4
charToDir 'L' = L
charToDir 'R' = R
charToDir 'U' = U
charToDir 'D' = D

updateTail :: Coord -> Coord -> Coord
updateTail tail@(x, y) head@(a, b)
  | touching tail head = tail
  | otherwise = (closer x a, closer y b)

closer :: (Num a) => a -> a -> a
closer x y = signum (y - x) + x

getHeads :: [Dir4] -> [Coord]
getHeads = scanl step4 (0, 0)

getTails :: [Coord] -> [[Coord]]
getTails = iterate (scanl updateTail (0, 0))

solve1 :: [String] -> Int
solve1 xs = length $ nub $ (tails !! 1)
  where
    dirs = concatMap (unsafeParse parseCommand) xs
    hs = getHeads dirs
    tails = getTails hs

solve2 :: [String] -> Int
solve2 xs = length $ nub $ (tails !! 9)
  where
    dirs = concatMap (unsafeParse parseCommand) xs
    hs = getHeads dirs
    tails = getTails hs

main :: IO()
main = mainWrapper "day9" solve1 solve2
