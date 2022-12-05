module Main where

import Data.List as L
import Data.Map as M
import Lib
import Parser

data Move = Move Int Int Int deriving (Show, Eq, Ord)

parseField :: Parser Char
parseField = (char '[' <|> char ' ') *> anyChar <* (char ']' <|> char ' ')

parseStack :: Parser [Char]
parseStack = sepBy parseField (char ' ')

parseStacks :: [String] -> M.Map Int String
parseStacks = M.fromList . zip [1 ..] . L.map (L.filter (/= ' ')) . transpose . L.map (unsafeParse parseStack)

parseMove :: Parser Move
parseMove = do
  string "move "
  amount <- integer
  string " from "
  from <- integer
  string " to "
  to <- integer
  return $ Move amount from to

parseMoves :: [String] -> [Move]
parseMoves = L.map (unsafeParse parseMove)

applyMove :: M.Map Int String -> Move -> M.Map Int String
applyMove stacks (Move amount from to) = M.insert to (toMove ++ toStack) $ M.insert from fromStackLeft stacks
  where
    fromStack = stacks M.! from
    toStack = stacks M.! to
    toMove = L.reverse $ L.take amount fromStack
    fromStackLeft = L.drop amount fromStack

takeFirst :: M.Map Int String -> String
takeFirst = L.map (head . snd) . M.toList

solve1 :: [String] -> String
solve1 xs = takeFirst $ L.foldl applyMove stacks moves
  where
    moves = parseMoves $ L.drop 10 xs
    stacks = parseStacks $ L.take 8 xs

applyMove' :: M.Map Int String -> Move -> M.Map Int String
applyMove' stacks (Move amount from to) = M.insert to (toMove ++ toStack) $ M.insert from fromStackLeft stacks
  where
    fromStack = stacks M.! from
    toStack = stacks M.! to
    toMove = L.take amount fromStack
    fromStackLeft = L.drop amount fromStack

solve2 :: [String] -> String
solve2 xs = takeFirst $ L.foldl applyMove' stacks moves
  where
    moves = parseMoves $ L.drop 10 xs
    stacks = parseStacks $ L.take 8 xs

main :: IO ()
main = mainWrapper "day5" solve1 solve2
