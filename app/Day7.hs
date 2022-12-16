module Main where

import Data.List
import qualified Data.Map as M
import Lib
import Parser

type Line = Either String [Either (Int, String) String]

parseCD :: Parser Line
parseCD = Left <$> (string "$ cd " *> stringLiteral <* char '\n')

parseFile :: Parser (Either (Int, String) String)
parseFile = Left <$> ((,) <$> natural <*> stringLiteral) <* char '\n'

parseDir :: Parser (Either (Int, String) String)
parseDir = Right <$> (string "dir " *> stringLiteral) <* char '\n'

parseLS :: Parser Line
parseLS = Right <$> (string "$ ls\n" *> many (parseFile <|> parseDir))

parseAll :: Parser [Line]
parseAll = many (parseCD <|> parseLS)

calcSum :: [Either (Int, a) a] -> Int
calcSum [] = 0
calcSum (Left (i, _) : xs) = i + calcSum xs
calcSum (_ : xs) = calcSum xs

calcSizes :: [String] -> [Line] -> [([String], Int)]
calcSizes _ [] = []
calcSizes _ (Left "/" : xs) = calcSizes [] xs
calcSizes pwd (Left ".." : xs) = calcSizes (tail pwd) xs
calcSizes pwd (Left dir : xs) = calcSizes (dir : pwd) xs
calcSizes pwd (Right list : xs) = (pwd, calcSum list) : calcSizes pwd xs

totalSizes dirs = M.elems $ M.fromListWith (+) [(d', n) | (d, n) <- dirs, d' <- tails d]

solve1 :: [String] -> Int
solve1 = sum . filter (<= 100_000) . totalSizes . calcSizes [] . unsafeParse parseAll . unlines

solve2 :: [String] -> Int
solve2 xs = head $ dropWhile (\x -> free + x < needed) allSizes
  where
    allSizes = sort . totalSizes . calcSizes [] . unsafeParse parseAll . unlines $ xs
    rootSize = last allSizes
    free = 70_000_000 - rootSize
    needed = 30_000_000

main :: IO ()
main = mainWrapper "day7" solve1 solve2
