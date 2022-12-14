module Main where

import Lib
import Parser
import Data.List

data Packet = Single Int | List [Packet] deriving (Show, Eq)

instance Ord Packet where
  compare (Single x) (List ys) = compare (List [Single x]) (List ys)
  compare (List xs) (Single y) = compare (List xs) (List [Single y])
  compare (Single x) (Single y) = compare x y
  compare (List xs) (List ys) = foldr (<>) (compare (length xs) (length ys)) (zipWith compare xs ys)

parsePacket :: Parser Packet
parsePacket = (Single <$> integer) <|> (char '[' *> (List <$> sepBy parsePacket (char ',')) <* char ']')

parsePair :: [String] -> [Packet]
parsePair (x : y : _) = [unsafeParse parsePacket x, unsafeParse parsePacket y]

comp :: [Packet] -> Ordering
comp (x:y:_) = compare x y

solve1 :: [String] -> Int
solve1 = sum . map fst . filter ((==LT) . snd ) . zip [1..] . map (comp . parsePair) . groupPairs

customFind :: (Num b) => b -> Packet -> [Packet] -> b
customFind i elem (x:xs)
  | elem == x = i
  | otherwise = customFind (i + 1) elem xs

solve2 :: [String] -> Int
solve2 xs = two * six
  where
    pairs = concatMap parsePair $ groupPairs (xs ++ ["", "[[2]]", "[[6]]"])
    sorted = sort pairs
    two = customFind 1 (List [List [Single 2]]) sorted
    six = customFind 1 (List [List [Single 6]]) sorted

main :: IO ()
main = mainWrapper "day13" solve1 solve2
