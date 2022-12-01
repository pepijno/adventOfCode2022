module Lib
  ( mainWrapper,
    inRange,
    groupPairs,
    rotate,
    count,
    mapFst,
    mapSnd,
    converge,
    converge',
    nSteps,
  )
where

import Control.Arrow
import Data.List.Split
import System.TimeIt

mainWrapper :: (Show a, Show b) => String -> ([String] -> a) -> ([String] -> b) -> IO ()
mainWrapper file f1 f2 = do
  contents <- lines <$> readFile ("./inputs/" ++ file ++ ".txt")
  timeIt $ print $ f1 contents
  timeIt $ print $ f2 contents

inRange :: (Ord a) => a -> a -> a -> Bool
inRange v min max = min <= v && v <= max

groupPairs :: [String] -> [[String]]
groupPairs = splitOn [""]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

rotate :: Int -> [a] -> [a]
rotate n xs = take len . drop (n `mod` len) . cycle $ xs
  where
    len = length xs

mapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapFst f = map (f . fst &&& snd)

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f = map (fst &&& f . snd)

converge :: (Eq a) => (a -> a) -> a -> a
converge f x =
  let x' = f x
   in if x' == x then x else converge f x'

converge' :: (Eq a) => (a -> a) -> a -> (Int, a)
converge' = converge'' 0 
  where
    converge'' i f x =
      let x' = f x
       in if x' == x then (i + 1, x) else converge'' (i + 1) f x'

nSteps :: Int -> (a -> a) -> (a -> a)
nSteps n f = (!! n) . iterate f
