module Main where

import Lib
import Grid
import qualified Data.Set as S
import qualified Data.Map as M

parseGrid' :: [String] -> S.Set Coord
parseGrid' xs = S.fromList [(col, row) | (row, line) <- zip [0 ..] xs, (col, b) <- zip [0 ..] line, b /= '.']

toConsider :: Coord -> Dir8 -> [Coord]
toConsider pos S = [step8 pos SW, step8 pos S, step8 pos SE]
toConsider pos N = [step8 pos NW, step8 pos N, step8 pos NE]
toConsider pos W = [step8 pos SW, step8 pos W, step8 pos NW]
toConsider pos E = [step8 pos SE, step8 pos E, step8 pos NE]

hasNoNeighbors :: S.Set Coord -> Coord -> Bool
hasNoNeighbors elves elf = S.null $ S.intersection elves $ S.fromList $ map (step8 elf) enumerate

getMove :: S.Set Coord -> [Dir8] -> Coord -> Coord
getMove elves (m1:m2:m3:m4:_) elf
  | hasNoNeighbors elves elf = elf
  | otherwise = head (move1 ++ move2 ++ move3 ++ move4 ++ [elf])
  where
    move1 = [step8 elf m1 | S.null (S.intersection (S.fromList $ toConsider elf m1) elves)]
    move2 = [step8 elf m2 | S.null (S.intersection (S.fromList $ toConsider elf m2) elves)]
    move3 = [step8 elf m3 | S.null (S.intersection (S.fromList $ toConsider elf m3) elves)]
    move4 = [step8 elf m4 | S.null (S.intersection (S.fromList $ toConsider elf m4) elves)]

getMoves :: (S.Set Coord, [Dir8]) -> (S.Set Coord, [Dir8])
getMoves (elves, moves) = (S.union ms' ms'', tail moves ++ [head moves])
  where
    ms = M.fromListWith (++) $ map (\x -> (getMove elves moves x, [x])) $ S.toList elves
    ms' = S.fromList $ M.keys $ M.map head $ M.filter ((==1) . length) ms
    ms'' = S.fromList $ concat $ M.elems $ M.filter ((/=1) . length) ms

minX :: S.Set Coord -> Int
minX elves = S.findMin $ S.map fst elves

maxX :: S.Set Coord -> Int
maxX elves = S.findMax $ S.map fst elves

minY :: S.Set Coord -> Int
minY elves = S.findMin $ S.map snd elves

maxY :: S.Set Coord -> Int
maxY elves = S.findMax $ S.map snd elves

solve1 :: [String] -> Int
solve1 xs = (maxX elves' - minX elves' + 1) * (maxY elves' - minY elves' + 1) - S.size elves'
  where
    elves = parseGrid' xs
    elves' = fst $ nSteps 10 getMoves (elves, [S, N, W, E])

converge'' = converge''' 0
  where
    converge''' i f (x, ms) =
      let (x', moves) = f (x, ms)
       in if x' == x then (i + 1, x) else converge''' (i + 1) f (x', moves)

solve2 :: [String] -> Int
solve2 xs = fst elves'
  where
    elves = parseGrid' xs
    elves' = converge'' getMoves (elves, [S, N, W, E])

main :: IO()
main = mainWrapper "day23" solve1 solve2
