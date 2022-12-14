module Main where

import Lib
import Parser
import Grid
import qualified Data.Set as S

parseCoord :: Parser Coord
parseCoord = (,) <$> (integer <* char ',') <*> integer

parseLine :: Parser [Coord]
parseLine = sepBy parseCoord (string " -> ")

makeRock :: [Coord] -> S.Set Coord
makeRock [] = S.empty
makeRock [x] = S.empty
makeRock ((a,b):(c,d):xs)
  | a > c = S.union (S.fromList [(x,b) | x <- [c..a]]) (makeRock ((c,d):xs))
  | a < c = S.union (S.fromList [(x,b) | x <- [a..c]]) (makeRock ((c,d):xs))
  | b > d = S.union (S.fromList [(a,x) | x <- [d..b]]) (makeRock ((c,d):xs))
  | b < d = S.union (S.fromList [(a,x) | x <- [b..d]]) (makeRock ((c,d):xs))
  | otherwise = S.union (S.fromList [(a,b)]) (makeRock ((c,d):xs))

fallGrainOfSand :: Int -> Coord -> S.Set Coord -> S.Set Coord
fallGrainOfSand height pos particles
  | S.member pos particles = particles
  | snd pos > height = particles
  | not $ S.member n particles = fallGrainOfSand height n particles
  | not $ S.member l particles = fallGrainOfSand height l particles
  | not $ S.member r particles = fallGrainOfSand height r particles
  | otherwise = S.insert pos particles
  where
    n = step8 pos N
    l = step8 pos NW
    r = step8 pos NE

solve1 :: [String] -> Int
solve1 xs = fst (converge' (fallGrainOfSand height (500,0)) rocks) - 1
  where
    rocks = foldl S.union S.empty . map (makeRock . unsafeParse parseLine) $ xs
    height = maximum $ map snd $ S.elems rocks

solve2 :: [String] -> Int
solve2 xs = fst (converge' (fallGrainOfSand height (500,0)) rocks') -1
  where
    rocks = foldl S.union S.empty . map (makeRock . unsafeParse parseLine) $ xs
    height = maximum (map snd $ S.elems rocks) + 2
    rocks' = S.union rocks $ S.fromList [(x, height) | x <- [0..1000]]

main :: IO()
main = mainWrapper "day14" solve1 solve2
