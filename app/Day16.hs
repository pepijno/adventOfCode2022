module Main where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Lib
import Parser

data Valve = Valve
  { rate :: Int,
    valves :: [String]
  }
  deriving (Show)

parseValve :: Parser (String, Valve)
parseValve = do
  string "Valve "
  valveId <- munch (/= ' ')
  string " has flow rate="
  rate <- integer
  string "; tunnels lead to valves " <|> string "; tunnel leads to valve "
  valves <- sepBy letters (string ", ")
  return (valveId, Valve rate valves)

doStep :: Int -> M.Map String Valve -> ((String, S.Set String), Int) -> [((String, S.Set String), Int)]
doStep 0 _ _ = []
doStep t vs ((curr, open), amount) = M.assocs $ M.fromListWith max $ withOpenValve ++ map (\x -> ((x, open), amount)) (valves $ vs M.! curr)
  where
    r = rate $ vs M.! curr
    withOpenValve = [((curr, S.insert curr open), amount + r * (t - 1)) | r /= 0, S.notMember curr open]

calcGraph :: M.Map String Valve -> Int -> [((String, S.Set String), Int)] -> M.Map (S.Set String) Int
calcGraph vs 0 currs = M.fromListWith max [(open, amount) | ((_, open), amount) <- currs]
calcGraph vs t currs = calcGraph vs (t - 1) $ M.assocs $ M.fromListWith max $ concatMap (doStep t vs) currs

solve1 :: [String] -> Int
solve1 xs = maximum $ calcGraph valves 30 [(("AA", S.empty), 0)]
  where
    valves = M.fromList $ map (unsafeParse parseValve) xs

solve2 :: [String] -> Int
solve2 xs = maximum [amount + amount' | (open, amount) : rest <- tails (M.assocs graph), (open', amount') <- rest, S.null (S.intersection open open')]
  where
    valves = M.fromList $ map (unsafeParse parseValve) xs
    graph = calcGraph valves 26 [(("AA", S.empty), 0)]

main :: IO ()
main = mainWrapper "day16" solve1 solve2
