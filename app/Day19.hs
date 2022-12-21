module Main where

import Data.List
import qualified Data.Map as M
import Lib
import Parser

data Blueprint = Blueprint
  { blueprintId :: Int,
    oreRobot :: Int,
    clayRobot :: Int,
    obsidianRobot :: (Int, Int),
    geodeRobot :: (Int, Int)
  }
  deriving (Show)

data Factory = Factory
  { blueprint :: Blueprint,
    oreRobots :: Int,
    clayRobots :: Int,
    obsidianRobots :: Int,
    geodeRobots :: Int,
    ore :: Int,
    clay :: Int,
    obsidian :: Int,
    geode :: Int,
    target :: String
  }
  deriving (Show)

parseBlueprint :: Parser Blueprint
parseBlueprint = do
  string "Blueprint "
  blueprintId <- integer
  string ": Each ore robot costs "
  oreRobot <- integer
  string " ore. Each clay robot costs "
  clayRobot <- integer
  string " ore. Each obsidian robot costs "
  obsidianOre <- integer
  string " ore and "
  obsidianClay <- integer
  string " clay. Each geode robot costs "
  geodeOre <- integer
  string " ore and "
  geodeObsidian <- integer
  string " obsidian."
  return Blueprint {blueprintId = blueprintId, oreRobot = oreRobot, clayRobot = clayRobot, obsidianRobot = (obsidianOre, obsidianClay), geodeRobot = (geodeOre, geodeObsidian)}

increment :: Factory -> Factory
increment factory = factory {ore = ore' + oreRobots factory, clay = clay' + clayRobots factory, obsidian = obsidian' + obsidianRobots factory, geode = geode' + geodeRobots factory}
  where
    ore' = ore factory
    clay' = clay factory
    obsidian' = obsidian factory
    geode' = geode factory

buyGeodeRobot :: Factory -> Factory
buyGeodeRobot factory = factory {ore = ore factory - geodeRobotOre, obsidian = obsidian factory - geodeRobotObsidian, geodeRobots = geodeRobots factory + 1}
  where
    (geodeRobotOre, geodeRobotObsidian) = geodeRobot $ blueprint factory

buyObsidianRobot :: Factory -> Factory
buyObsidianRobot factory = factory {ore = ore factory - obsidianRobotOre, clay = clay factory - obsidianRobotClay, obsidianRobots = obsidianRobots factory + 1}
  where
    (obsidianRobotOre, obsidianRobotClay) = obsidianRobot $ blueprint factory

buyClayRobot :: Factory -> Factory
buyClayRobot factory = factory {ore = ore factory - clayRobotOre, clayRobots = clayRobots factory + 1}
  where
    clayRobotOre = clayRobot $ blueprint factory

buyOreRobot :: Factory -> Factory
buyOreRobot factory = factory {ore = ore factory - oreRobotOre, oreRobots = oreRobots factory + 1}
  where
    oreRobotOre = oreRobot $ blueprint factory

calcBest :: Factory -> (Bool, Bool, Bool) -> Int -> Int -> Int
calcBest factory _ best 0 = max best $ geode factory
calcBest factory (canMakeOre, canMakeClay, canMakeObsidian) best time
  | (time * (time - 1)) `div` 2 + geode factory + geodeRobots factory * time <= best = best
  | ore factory >= geodeRobotOre && obsidian factory >= geodeRobotObsidian = calcBest (buyGeodeRobot inc) (False, False, False) best (time - 1)
  | otherwise = foldl (\b f -> calcBest f (False, False, False) b (time - 1)) (calcBest inc (makeOre, makeClay, makeObsidian) best (time - 1)) (withObsidian ++ withClay ++ withOre)
  where
    inc = increment factory
    bp = blueprint factory
    (geodeRobotOre, geodeRobotObsidian) = geodeRobot bp
    (obsidianRobotOre, obsidianRobotClay) = obsidianRobot bp
    clayRobotOre = clayRobot bp
    oreRobotOre = oreRobot bp
    makeObsidian = ore factory >= obsidianRobotOre && clay factory >= obsidianRobotClay
    makeClay = ore factory >= clayRobotOre
    makeOre = ore factory >= oreRobotOre
    withObsidian = [buyObsidianRobot inc | not canMakeObsidian && makeObsidian && obsidianRobots factory < geodeRobotObsidian]
    withClay = [buyClayRobot inc | not canMakeClay && makeClay && clayRobots factory < obsidianRobotClay]
    withOre = [buyOreRobot inc | not canMakeOre && makeOre && oreRobots factory < maximum [geodeRobotOre, obsidianRobotOre, clayRobotOre, oreRobotOre]]

createFactory :: Blueprint -> Factory
createFactory blueprint = Factory {blueprint = blueprint, oreRobots = 1, clayRobots = 0, obsidianRobots = 0, geodeRobots = 0, ore = 0, clay = 0, obsidian = 0, geode = 0, target = "ore"}

solve1 :: [String] -> Int
solve1 = sum . zipWith (*) [1 ..] . map ((\f -> calcBest f (False, False, False) 0 24) . createFactory . unsafeParse parseBlueprint)

solve2 :: [String] -> Int
solve2 = product . take 3 . map ((\f -> calcBest f (False, False, False) 0 32) . createFactory . unsafeParse parseBlueprint)

main :: IO ()
main = mainWrapper "day19" solve1 solve2
