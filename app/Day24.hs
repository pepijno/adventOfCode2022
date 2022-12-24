module Main where

import Data.Bifunctor (second)
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Grid
import Lib

data Move = Move Dir4 | Wait deriving (Show)

parseGrid' :: (Ord b) => (Char -> Maybe b) -> [String] -> S.Set (Coord, b)
parseGrid' parser xs = S.map (second fromJust) $ S.filter (isJust . snd) $ S.map (second parser) $ S.fromList [((col, row), b) | (row, line) <- zip [0 ..] xs, (col, b) <- zip [0 ..] line]

parseBlizzard :: Char -> Maybe Dir4
parseBlizzard '^' = Just D
parseBlizzard '>' = Just R
parseBlizzard 'v' = Just U
parseBlizzard '<' = Just L
parseBlizzard _ = Nothing

wrapCoord :: Coord -> Coord -> Coord
wrapCoord (sizeX, sizeY) (x, y) = (if x == -1 then sizeX - 1 else if x == sizeX then 0 else x, if y == -1 then sizeY - 1 else if y == sizeY then 0 else y)

moveBlizzards :: Coord -> S.Set (Coord, Dir4) -> S.Set (Coord, Dir4)
moveBlizzards size = S.map moveBlizzard
  where
    moveBlizzard (pos, dir) = (wrapCoord size $ step4 pos dir, dir)

doMove :: Coord -> Move -> Coord
doMove pos Wait = pos
doMove pos (Move dir) = step4 pos dir

isOutside :: Coord -> Coord -> Bool
isOutside (sizeX, sizeY) (x, y)
  | (x, y) == (0, -1) = False
  | (x, y) == (sizeX - 1, sizeY) = False
  | otherwise = x < 0 || y < 0 || x >= sizeX || y >= sizeY

getMoves :: S.Set (Coord, Dir4) -> Coord -> Coord -> [Coord]
getMoves grid size pos = ns'
  where
    ns = [Wait, Move U, Move L, Move D, Move R]
    ns' = filter (\x -> not (isOutside size x) && all (\dir -> not (S.member (x, dir) grid)) [U, D, L, R]) $ map (doMove pos) ns

bfs :: Coord -> Coord -> S.Set (Coord, Dir4) -> Int -> [Coord] -> (S.Set (Coord, Dir4), Int)
bfs size@(sizeX, sizeY) end grid n poss
  | end `elem` poss = (grid, n)
  | otherwise = bfs size end grid' (n + 1) poss'
  where
    grid' = moveBlizzards size grid
    poss' = nub $ concatMap (getMoves grid' size) poss

start :: Coord
start = (0, -1)

solve1 :: [String] -> Int
solve1 xs = snd $ bfs size end grid 0 [start]
  where
    grid = parseGrid' parseBlizzard $ map (init . tail) $ init $ tail xs
    sizeX = maximum (map (fst . fst) $ S.elems grid) + 1
    sizeY = maximum (map (snd . fst) $ S.elems grid) + 1
    size = (sizeX, sizeY)
    end = (sizeX - 1, sizeY)

solve2 :: [String] -> Int
solve2 xs = steps''
  where
    grid = parseGrid' parseBlizzard $ map (init . tail) $ init $ tail xs
    sizeX = maximum (map (fst . fst) $ S.elems grid) + 1
    sizeY = maximum (map (snd . fst) $ S.elems grid) + 1
    size = (sizeX, sizeY)
    end = (sizeX - 1, sizeY)
    (grid', steps) = bfs size end grid 0 [start]
    (grid'', steps') = bfs size start grid' steps [end]
    steps'' = snd $ bfs size end grid'' steps' [start]

main :: IO ()
main = mainWrapper "day24" solve1 solve2
