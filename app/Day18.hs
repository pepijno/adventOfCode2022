module Main where

import Lib
import Parser
import qualified Data.Set as S

type Coord = (Int, Int, Int)

parseCoord :: Parser Coord
parseCoord = toCoord <$> sepBy integer (char ',')

toCoord :: [Int] -> Coord
toCoord (x:y:z:_) = (x,y,z)

neighbors :: Coord -> [Coord]
neighbors (x, y, z) = [(x - 1, y, z), (x + 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1)]

getX :: Coord -> Int
getX (x, _, _) = x
getY :: Coord -> Int
getY (_, y, _) = y
getZ :: Coord -> Int
getZ (_, _, z) = z

solve1 :: [String] -> Int
solve1 xs = length [c | c <- cubes, n <- neighbors c, S.notMember n cs]
  where
    cubes = map (unsafeParse parseCoord) xs
    cs = S.fromList cubes

bfs :: S.Set Coord -> S.Set Coord -> [Coord] -> Int -> Int
bfs _ _ [] n = n
bfs cubes had (curr:queue) n
  | curr `S.member` had = bfs cubes had queue n
  | otherwise = bfs cubes (curr `S.insert` had) (queue ++ ns) (n + s)
  where
    nb = neighbors curr
    ns = filter (`S.notMember` cubes) $ filter (`S.notMember` had) nb
    s = length $ filter (`S.member` cubes) nb

makeSides :: [Coord] -> [Coord]
makeSides cubes =
  [(minX - 4, y, z) | y <- [(minY - 4)..(maxY + 4)], z <- [(minZ - 4)..(maxZ + 4)]] ++
  [(maxX + 4, y, z) | y <- [(minY - 4)..(maxY + 4)], z <- [(minZ - 4)..(maxZ + 4)]] ++
  [(x, minY - 4, z) | x <- [(minX - 4)..(maxX + 4)], z <- [(minZ - 4)..(maxZ + 4)]] ++
  [(x, maxY + 4, z) | x <- [(minX - 4)..(maxX + 4)], z <- [(minZ - 4)..(maxZ + 4)]] ++
  [(x, y, minZ - 4) | x <- [(minX - 4)..(maxX + 4)], y <- [(minY - 4)..(maxY + 4)]] ++
  [(x, y, maxZ + 4) | x <- [(minX - 4)..(maxX + 4)], y <- [(minY - 4)..(maxY + 4)]]
  where
    maxX = maximum $ map getX cubes
    minX = minimum $ map getX cubes
    maxY = maximum $ map getX cubes
    minY = minimum $ map getX cubes
    maxZ = maximum $ map getX cubes
    minZ = minimum $ map getX cubes

solve2 :: [String] -> Int
solve2 xs = bfs cs sides [(1, 1, 1)] 0
  where
    cubes = map (unsafeParse parseCoord) xs
    cs = S.fromList cubes
    sides = S.fromList $ makeSides cubes

main :: IO()
main = mainWrapper "day18" solve1 solve2
