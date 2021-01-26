{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Data.List (transpose)
import Text.RawString.QQ

data Reindeer = Reindeer
  { speed :: Int,
    flyTime :: Int,
    restTime :: Int
  }
  deriving (Show)

data Racer = Racer
  { reindeer :: Reindeer,
    isResting :: Bool,
    travelled :: Int
  }
  deriving (Show)

samples :: [Reindeer]
samples =
  [ Reindeer {speed = 14, flyTime = 10, restTime = 127},
    Reindeer {speed = 16, flyTime = 11, restTime = 162}
  ]

newRacer :: Reindeer -> Racer
newRacer Reindeer {speed, flyTime, restTime} =
  Racer
    { reindeer = Reindeer {speed, flyTime, restTime},
      isResting = False,
      travelled = 0
    }

raceNTimes :: Int -> Racer -> Racer
raceNTimes 0 racer = racer
raceNTimes timeRemaining racer
  | isResting =
    if timeRemaining <= restTime
      then racer {isResting = False, travelled}
      else raceNTimes (timeRemaining - restTime) racer {isResting = False, travelled}
  | otherwise =
    if timeRemaining <= flyTime
      then racer {isResting = False, travelled = travelled + (speed * timeRemaining)}
      else raceNTimes (timeRemaining - flyTime) $ racer {isResting = True, travelled = travelled + (speed * flyTime)}
  where
    Racer {reindeer = Reindeer {speed, flyTime, restTime}, isResting, travelled} = racer

part1 :: IO ()
part1 = print $ maximum $ map (travelled . raceNTimes 2503 . newRacer . parseLine) (lines input)

scoreRace :: [Racer] -> [Int]
scoreRace racers =
  let maxDistance = maximum $ map travelled racers
   in map (\Racer {travelled} -> if travelled == maxDistance then 1 else 0) racers

part2 :: Int
part2 =
  let racers = map (newRacer . parseLine) $ lines input
      raceAtEachSecond = map (\n -> map (raceNTimes n) racers) [1 .. 2503]
   in maximum $ map sum $ transpose $ map scoreRace raceAtEachSecond

run :: IO ()
run = print part2

parseLine :: String -> Reindeer
parseLine line =
  let ws = words line
   in Reindeer {speed = read $ ws !! 3, flyTime = read $ ws !! 6, restTime = read $ ws !! 13}

input :: String
input =
  [r|Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.
Cupid can fly 22 km/s for 2 seconds, but then must rest for 41 seconds.
Rudolph can fly 11 km/s for 5 seconds, but then must rest for 48 seconds.
Donner can fly 28 km/s for 5 seconds, but then must rest for 134 seconds.
Dasher can fly 4 km/s for 16 seconds, but then must rest for 55 seconds.
Blitzen can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.
Prancer can fly 3 km/s for 21 seconds, but then must rest for 40 seconds.
Comet can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
Vixen can fly 18 km/s for 5 seconds, but then must rest for 84 seconds.|]
