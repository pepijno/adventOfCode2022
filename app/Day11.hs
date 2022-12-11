module Main where

import Lib
import Parser
import Data.List
import qualified Data.Map as M

data Monkey = Monkey {
  items :: [Int],
  operation :: Int -> Int,
  test :: Int -> Int,
  itemsThrown :: Integer,
  divisor :: Int
}

parseOperation :: Parser (Int -> Int)
parseOperation = ((+) <$> (string "old + " *> integer)) <|> ((*) <$> (string "old * " *> integer)) <|> ((string "old * old") *> pure (^2))

parseMonkey :: Parser (Int, Monkey)
parseMonkey = do
  string "Monkey "
  i <- natural
  string ":\n  Starting items: "
  items <- sepBy natural (string ", ")
  string "\n  Operation: new = "
  operation <- parseOperation
  string "\n  Test: divisible by "
  d <- integer
  string "\n    If true: throw to monkey "
  m1 <- integer
  string "\n    If false: throw to monkey "
  m2 <- integer
  string "\n"
  return $ (i, Monkey { items = items, operation = operation, test = \x -> if x `mod` d == 0 then m1 else m2, itemsThrown = 0, divisor = d })

parseMonkeys :: Parser [(Int, Monkey)]
parseMonkeys = sepBy parseMonkey (string "\n")

processMonkey :: Int -> (Int -> Int) -> M.Map Int Monkey -> Int -> M.Map Int Monkey
processMonkey maxDiv worryDecrease monkeys index = foldl (processItem) (M.insert index m monkeys) is
  where
    monkey = monkeys M.! index
    is = items monkey
    m = monkey{ items = [], itemsThrown = (itemsThrown monkey) + toInteger (length is) }
    newWorry item = (worryDecrease $ (operation monkey) item) `mod` maxDiv
    throwsTo item = (test monkey) $ newWorry item
    addToMonkey mk worry = mk{ items = (items mk) ++ [worry] }
    processItem mks item = let i = throwsTo item
                           in M.insert i (addToMonkey (mks M.! i) (newWorry item)) mks

doOneRound :: (Int -> Int) -> M.Map Int Monkey -> M.Map Int Monkey
doOneRound worryDecrease monkeys = foldl (processMonkey maxDiv worryDecrease) monkeys $ [0..(M.size monkeys - 1)]
  where
    maxDiv = product $ map (divisor . snd) $ M.toList monkeys

solve1 :: [String] -> Integer
solve1 xs = product . take 2 . reverse . sort . map (itemsThrown . snd) . M.toList $ nSteps 20 (doOneRound (`div` 3)) monkeys
  where
    monkeys = M.fromList . unsafeParse parseMonkeys . unlines $ xs

solve2 :: [String] -> Integer
solve2 xs = product . take 2 . reverse . sort . map (itemsThrown . snd) . M.toList $ nSteps 10000 (doOneRound id) monkeys
  where
    monkeys = M.fromList . unsafeParse parseMonkeys . unlines $ xs

main :: IO()
main = mainWrapper "day11" solve1 solve2
