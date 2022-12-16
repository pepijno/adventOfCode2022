module Main where

import qualified Data.Set as S
import Grid
import Lib
import Parser

data Sensor = Sensor
  { pos :: Coord,
    closest :: Coord
  }
  deriving (Show)

parseSensor :: Parser Sensor
parseSensor = do
  string "Sensor at x="
  x <- integer
  string ", y="
  y <- integer
  string ": closest beacon is at x="
  x' <- integer
  string ", y="
  y' <- integer
  return Sensor {pos = (x, y), closest = (x', y')}

rowSlice :: Int -> Sensor -> [(Int, Int)]
rowSlice row Sensor {pos = (x, y), closest = (x', y')} = [(x - dx, x + dx) | dx >= 0]
  where
    dy = abs (row - y)
    dx = manhattan (x, y) (x', y') - dy

union :: (Int, Int) -> (Int, Int) -> (Int, Int)
union (x, y) (x', y') = (min x x', max y y')

range :: (Int, Int) -> [Int]
range (x, y) = [x .. y]

solve1 :: [String] -> Int
solve1 xs = length [(c, row) | c <- range withoutBeaconBox, (c, row) `notElem` beacons]
  where
    sensors = map (unsafeParse parseSensor) xs
    beacons = map closest sensors
    row = 2_000_000
    withoutBeaconBox = foldl1 union $ concatMap (rowSlice row) sensors

outsideSensor :: Sensor -> [Coord]
outsideSensor Sensor {pos = p@(a, b), closest = c} =
  [(a - i, b - d - 1 + i) | i <- [0 .. (d + 1)]]
    ++ [(a + i, b - d - 1 + i) | i <- [1 .. (d + 1)]]
    ++ [(a - i, b + d + 1 - i) | i <- [0 .. d]]
    ++ [(a + i, b + d + 1 - i) | i <- [1 .. d]]
  where
    d = manhattan p c

isOutOfRange :: Coord -> Sensor -> Bool
isOutOfRange x Sensor {pos = p, closest = c} = manhattan x p > d
  where
    d = manhattan p c

isBounded :: Int -> Coord -> Bool
isBounded m (x, y) = 0 <= x && x <= m && 0 <= y && y <= m

tuningFrequency :: Coord -> Int
tuningFrequency (x, y) = 4_000_000 * x + y

solve2 :: [String] -> Int
solve2 xs = tuningFrequency $ head $ filter (isBounded 4_000_000) . filter (\x -> all (isOutOfRange x) sensors) $ concatMap outsideSensor sensors
  where
    sensors = map (unsafeParse parseSensor) xs

main :: IO ()
main = mainWrapper "day15" solve1 solve2
