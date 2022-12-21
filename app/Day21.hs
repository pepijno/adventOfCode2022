module Main where

import qualified Data.Map as M
import Lib
import Parser

data Monkey = Number Int | Operation (Int -> Int -> Int) (Int -> Int -> Int) Bool String String | Unsure

parseNumber :: Parser Monkey
parseNumber = Number <$> integer

parseAddition :: Parser Monkey
parseAddition = Operation (+) (-) True <$> (letters <* string " + ") <*> letters

parseSubtraction :: Parser Monkey
parseSubtraction = Operation (-) (+) False <$> (letters <* string " - ") <*> letters

parseMultiplication :: Parser Monkey
parseMultiplication = Operation (*) div True <$> (letters <* string " * ") <*> letters

parseDivision :: Parser Monkey
parseDivision = Operation div (*) False <$> (letters <* string " / ") <*> letters

parseMonkey :: Parser (String, Monkey)
parseMonkey = (,) <$> (munch (/= ':') <* string ": ") <*> (parseNumber <|> parseAddition <|> parseSubtraction <|> parseMultiplication <|> parseDivision)

calcMonkey :: M.Map String Monkey -> String -> (Int, M.Map String Monkey)
calcMonkey monkeys monkey = case m of
  (Number x) -> (x, monkeys)
  _ -> (n, monkeys''')
  where
    m = monkeys M.! monkey
    (Operation op _ _ n1 n2) = m
    (i1, monkeys') = calcMonkey monkeys n1
    (i2, monkeys'') = calcMonkey monkeys' n2
    n = op i1 i2
    monkeys''' = M.insert monkey (Number n) monkeys''

solve1 :: [String] -> Int
solve1 = fst . flip calcMonkey "root" . M.fromList . map (unsafeParse parseMonkey)

isUnsure :: M.Map String Monkey -> String -> Bool
isUnsure monkeys monkey = case m of
  (Number _) -> False
  Unsure -> True
  _ -> unsure1 || unsure2
  where
    m = monkeys M.! monkey
    (Operation _ _ _ n1 n2) = m
    unsure1 = isUnsure monkeys n1
    unsure2 = isUnsure monkeys n2

calcUnsure :: M.Map String Monkey -> String -> Int -> Int
calcUnsure monkeys monkey target
  | Unsure <- m = target
  | unsure1 = calcUnsure monkeys2 n1 target1
  | unsure2 = calcUnsure monkeys1 n2 target2
  where
    m = monkeys M.! monkey
    (Operation op invOp isCommutative n1 n2) = m
    unsure1 = isUnsure monkeys n1
    unsure2 = isUnsure monkeys n2
    (i1, monkeys1) = calcMonkey monkeys n1
    (i2, monkeys2) = calcMonkey monkeys n2
    target1 = invOp target i2
    target2 = if isCommutative then invOp target i1 else op i1 target

replaceRoot :: M.Map String Monkey -> M.Map String Monkey
replaceRoot monkeys = M.insert "root" (Operation (-) (+) True n1 n2) monkeys
  where
    root = monkeys M.! "root"
    (Operation _ _ _ n1 n2) = root

solve2 :: [String] -> Int
solve2 xs = calcUnsure unsure "root" 0
  where
    monkeys = M.fromList $ map (unsafeParse parseMonkey) xs
    unsure = replaceRoot $ M.insert "humn" Unsure monkeys

main :: IO ()
main = mainWrapper "day21" solve1 solve2
