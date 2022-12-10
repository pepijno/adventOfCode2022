module Main where

import Lib
import Parser
import Data.List.Split

data Command = Noop | Addx Int deriving (Show, Ord, Eq)
data Cpu = Cpu {
  clock :: Int,
  register :: Int,
  pixels :: String
} deriving (Show)

parseNoop :: Parser [Command]
parseNoop = do
  string "noop"
  return [Noop]

parseAddx :: Parser [Command]
parseAddx = do
  string "addx "
  x <- integer
  return [Noop, Addx x]

parseCommand :: Parser [Command]
parseCommand = parseNoop <|> parseAddx

doCommand :: Cpu -> Command -> Cpu
doCommand cpu@(Cpu clock reg pixels) c = case c of
  Noop -> cpu{ clock = clock + 1, pixels = pixel:pixels }
  (Addx x) -> cpu{ clock = clock + 1, register = reg + x, pixels = pixel:pixels }
  where
    pixel = if abs ((clock `mod` 40) - reg) <= 1 then '#' else '.'

solve1 :: [String] -> Int
solve1 xs = sum $ map (\x -> (clock x * register x)) $ map (\x -> cycles !! (x - 1)) signalCycles
  where
    commands = concatMap (unsafeParse parseCommand) xs
    cycles = scanl doCommand (Cpu 1 1 []) commands
    signalCycles = [20, 60, 100, 140, 180, 220]

solve2 :: [String] -> String
solve2 xs = unlines $ chunksOf 40 $ reverse $ pixels $ last cycles
  where
    commands = concatMap (unsafeParse parseCommand) xs
    cycles = scanl doCommand (Cpu 0 1 []) commands

main :: IO()
main = mainWrapper "day10" solve1 solve2
