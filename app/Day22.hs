module Main where

import Lib
import Grid
import qualified Data.Map as M
import Parser

data Move = I Int | T Char deriving (Show)

parseRoute :: Parser [Move]
parseRoute = many ((I <$> integer) <|> (T <$> (char 'L' <|> char 'R')))

ws :: M.Map (Coord, Dir4) (Coord, Dir4)
ws = M.fromList $ [(((100 + x, 49), U), ((100 + x, 0), U)) | x <- [0..49]]
  ++ [(((100 + x, 0), D), ((100 + x, 49), D)) | x <- [0..49]]
  ++ [(((50 + x, 149), U), ((50 + x, 0), U)) | x <- [0..49]]
  ++ [(((50 + x, 0), D), ((50 + x, 149), D)) | x <- [0..49]]
  ++ [(((x, 100), D), ((x, 199), D)) | x <- [0..49]]
  ++ [(((x, 199), U), ((x, 100), U)) | x <- [0..49]]
  ++ [(((50, x), L), ((149, x), L)) | x <- [0..49]]
  ++ [(((149, x), R), ((50, x), R)) | x <- [0..49]]
  ++ [(((50, 50 + x), L), ((99, 50 + x), L)) | x <- [0..49]]
  ++ [(((99, 50 + x), R), ((50, 50 + x), R)) | x <- [0..49]]
  ++ [(((0, 100 + x), L), ((99, 100 + x), L)) | x <- [0..49]]
  ++ [(((99, 100 + x), R), ((0, 100 + x), R)) | x <- [0..49]]
  ++ [(((0, 150 + x), L), ((49, 150 + x), L)) | x <- [0..49]]
  ++ [(((49, 150 + x), R), ((0, 150 + x), R)) | x <- [0..49]]

wsCube :: M.Map (Coord, Dir4) (Coord, Dir4)
wsCube = M.fromList $ [(((100 + x, 49), U), ((99, 50 + x), L)) | x <- [0..49]]
  ++ [(((99, 50 + x), R), ((100 + x, 49), D)) | x <- [0..49]]
  ++ [(((50, 50 + x), L), ((x, 100), U)) | x <- [0..49]]
  ++ [(((x, 100), D), ((50, 50 + x), R)) | x <- [0..49]]
  ++ [(((149, x), R), ((99, 149 - x), L)) | x <- [0..49]]
  ++ [(((99, 149 - x), R), ((149, x), L)) | x <- [0..49]]
  ++ [(((50, x), L), ((0, 149 - x), R)) | x <- [0..49]]
  ++ [(((0, 149 - x), L), ((50, x), R)) | x <- [0..49]]
  ++ [(((50 + x, 149), U), ((49, 150 + x), L)) | x <- [0..49]]
  ++ [(((49, 150 + x), R), ((50 + x, 149), D)) | x <- [0..49]]
  ++ [(((100 + x, 0), D), ((x, 199), D)) | x <- [0..49]]
  ++ [(((x, 199), U), ((100 + x, 0), U)) | x <- [0..49]]
  ++ [(((50 + x, 0), D), ((0, 150 + x), R)) | x <- [0..49]]
  ++ [(((0, 150 + x), L), ((50 + x, 0), U)) | x <- [0..49]]

parseGrid' :: [String] -> M.Map Coord Char
parseGrid' xs = M.fromList [((col, row), b) | (row, line) <- zip [0 ..] xs, (col, b) <- zip [0 ..] line]

turnLeft :: Dir4 -> Dir4
turnLeft U = R
turnLeft R = D
turnLeft D = L
turnLeft L = U

turnRight :: Dir4 -> Dir4
turnRight U = L
turnRight R = U
turnRight D = R
turnRight L = D

doStep :: M.Map (Coord, Dir4) (Coord, Dir4) -> M.Map Coord Char -> Coord -> Dir4 -> Int -> (Coord, Dir4)
doStep _ _ curr dir 0 = (curr, dir)
doStep wraps m curr dir n
  | (m M.! next) == '#' = (curr, dir)
  | otherwise = doStep wraps m next dir' (n - 1)
  where
    (next, dir') = if M.member (curr, dir) wraps then wraps M.! (curr, dir) else (step4 curr dir, dir)

doMove :: M.Map (Coord, Dir4) (Coord, Dir4) -> M.Map Coord Char -> Coord -> Dir4 -> Move -> (Coord, Dir4)
doMove wraps m curr dir (I n) = doStep wraps m curr dir n
doMove _ _ curr dir (T 'L') = (curr, turnLeft dir)
doMove _ _ curr dir (T 'R') = (curr, turnRight dir)

password :: (Coord, Dir4) -> Int
password ((col, row), dir) = 1000 * (row + 1) + 4 * (col + 1) + d
  where
    d = case dir of
      U -> 1
      R -> 0
      D -> 3
      L -> 2

solve1 :: [String] -> Int
solve1 xs = password $ foldl (\(curr, dir) move -> doMove ws map' curr dir move) (start, R) route
  where
    m:r:_ = groupPairs xs 
    route = unsafeParse parseRoute $ head r
    map' = M.filter (/=' ') $ parseGrid' m
    start = fst $ head $ M.toAscList $ M.filterWithKey (\k _ -> (==0) $ snd k) map'

solve2 :: [String] -> Int
solve2 xs = password $ foldl (\(curr, dir) move -> doMove wsCube map' curr dir move) (start, R) route
  where
    m:r:_ = groupPairs xs 
    route = unsafeParse parseRoute $ head r
    map' = M.filter (/=' ') $ parseGrid' m
    start = fst $ head $ M.toAscList $ M.filterWithKey (\k _ -> (==0) $ snd k) map'

main :: IO()
main = mainWrapper "day22" solve1 solve2
