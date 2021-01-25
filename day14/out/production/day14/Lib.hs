{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

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
raceNTimes timeRemaining Racer {reindeer = Reindeer {speed, flyTime, restTime}, isResting, travelled}
  | isResting =
    let next = nextRacer False travelled
     in if timeRemaining <= restTime
          then next
          else raceNTimes (timeRemaining - restTime) next
  | otherwise =
    if timeRemaining <= flyTime
      then nextRacer False (travelled + (speed * timeRemaining))
      else raceNTimes (timeRemaining - flyTime) $ nextRacer True (travelled + (speed * flyTime))
  where
    nextRacer isResting travelled = Racer {reindeer = Reindeer {speed, flyTime, restTime}, isResting, travelled}

run :: IO ()
run = print $ maximum $ map travelled $ map (raceNTimes 2503) $ map newRacer $ map parseLine (lines input)

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
