{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Combinatorics (variate)
import Data.List (intercalate, sortOn)
import qualified Data.Ord

run :: IO ()
run = part2

part1 :: IO ()
part1 = do
  print $ minimum myStatss
  where
    myStatss = [cost | (me, cost) <- map myStats combinationsOfGearForMe, doIWin me bossStats]

part2 :: IO ()
part2 = do
  print $ maximum myStatss
  where
    myStatss = [cost | (me, cost) <- map myStats combinationsOfGearForMe, not $ doIWin me bossStats]

combinationsOfGearForMe :: [[Item]]
combinationsOfGearForMe =
  [ ringSet ++ armorSet ++ weaponsSet
    | ringSet <- concatMap (`variate` rings) [0, 1, 2],
      armorSet <- concatMap (`variate` armor) [0, 1],
      weaponsSet <- concatMap (`variate` weapons) [1]
  ]

doIWin :: Fighter -> Fighter -> Bool
doIWin me boss = numHitsForBoss <= numHitsForMe
  where
    numHitsForBoss = numHitsToKO boss me
    numHitsForMe = numHitsToKO me boss

numHitsToKO :: Fighter -> Fighter -> Int -- Defender -> Attacker -> Num Hits
numHitsToKO Fighter {hp, shield} Fighter {attack} = hp `ceilDiv` attackAfterShield
  where
    attackAfterShield = max 1 $ attack - shield

data Item = Item
  { cost :: Int,
    dmg :: Int,
    guard :: Int
  }
  deriving (Show)

data Fighter = Fighter
  { hp :: Int,
    attack :: Int,
    shield :: Int
  }
  deriving (Show)

bossStats :: Fighter
bossStats = Fighter {hp = 109, attack = 8, shield = 2}

myStats :: [Item] -> (Fighter, Int)
myStats gear = (Fighter {hp = 100, attack = dmg, shield = guard}, cost)
  where
    Item {cost, dmg, guard} = foldl add Item {cost = 0, dmg = 0, guard = 0} gear

add :: Item -> Item -> Item
add Item {cost = cost1, dmg = dmg1, guard = guard1} Item {cost = cost2, dmg = dmg2, guard = guard2} =
  Item
    { cost = cost1 + cost2,
      dmg = dmg1 + dmg2,
      guard = guard1 + guard2
    }

weapons :: [Item]
weapons =
  [ Item {cost = 8, dmg = 4, guard = 0},
    Item {cost = 10, dmg = 5, guard = 0},
    Item {cost = 25, dmg = 6, guard = 0},
    Item {cost = 40, dmg = 7, guard = 0},
    Item {cost = 74, dmg = 8, guard = 0}
  ]

armor :: [Item]
armor =
  [ Item {cost = 13, dmg = 0, guard = 1},
    Item {cost = 31, dmg = 0, guard = 2},
    Item {cost = 53, dmg = 0, guard = 3},
    Item {cost = 75, dmg = 0, guard = 4},
    Item {cost = 102, dmg = 0, guard = 5}
  ]

rings :: [Item]
rings =
  [ Item {cost = 25, dmg = 1, guard = 0},
    Item {cost = 50, dmg = 2, guard = 0},
    Item {cost = 100, dmg = 3, guard = 0},
    Item {cost = 20, dmg = 0, guard = 1},
    Item {cost = 40, dmg = 0, guard = 2},
    Item {cost = 80, dmg = 0, guard = 3}
  ]

ceilDiv :: Int -> Int -> Int
ceilDiv l r = divided + if hasRemainder then 1 else 0
  where
    hasRemainder = remainder /= 0
    (divided, remainder) = l `divMod` r

showMany :: Show a => [a] -> String
showMany xs = intercalate "\n\n" $ map show xs
