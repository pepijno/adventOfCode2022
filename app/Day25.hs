module Main where

import Lib

parseSnafuDigit :: Char -> Int
parseSnafuDigit '=' = -2
parseSnafuDigit '-' = -1
parseSnafuDigit '0' = 0
parseSnafuDigit '1' = 1
parseSnafuDigit '2' = 2

toSnafuDigit :: Int -> Char
toSnafuDigit (-2) = '='
toSnafuDigit (-1) = '-'
toSnafuDigit 0 = '0'
toSnafuDigit 1 = '1'
toSnafuDigit 2 = '2'

parseSnafu :: String -> Int
parseSnafu xs = sum $ zipWith (\x y -> y * 5 ^ x) [0..] $ map parseSnafuDigit $ reverse xs

toSnafu :: Int -> [Int]
toSnafu 0 = []
toSnafu x = m':toSnafu (x' `div` 5)
  where
    m = x `mod` 5
    x' = if (m == 4 || m == 3) then x + 5 else x
    m' = if (m == 4 || m == 3) then m - 5 else m

solve1 :: [String] -> String
solve1 = reverse . map toSnafuDigit . toSnafu . sum . map parseSnafu

solve2 :: [String] -> String
solve2 = solve1

main :: IO()
main = mainWrapper "day25" solve1 solve2
