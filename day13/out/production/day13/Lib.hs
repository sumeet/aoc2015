{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Data.Bifunctor (second)
import Data.List (permutations)
import Data.Map (Map, fromList, fromListWith, insert, keys, toList, (!))
import Text.RawString.QQ

type Person = String

type SeatmateQuality = Map Person Int

type Preferences = Map Person SeatmateQuality

sample :: String
sample =
  [r|Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.|]

input :: String
input =
  [r|Alice would gain 2 happiness units by sitting next to Bob.
Alice would gain 26 happiness units by sitting next to Carol.
Alice would lose 82 happiness units by sitting next to David.
Alice would lose 75 happiness units by sitting next to Eric.
Alice would gain 42 happiness units by sitting next to Frank.
Alice would gain 38 happiness units by sitting next to George.
Alice would gain 39 happiness units by sitting next to Mallory.
Bob would gain 40 happiness units by sitting next to Alice.
Bob would lose 61 happiness units by sitting next to Carol.
Bob would lose 15 happiness units by sitting next to David.
Bob would gain 63 happiness units by sitting next to Eric.
Bob would gain 41 happiness units by sitting next to Frank.
Bob would gain 30 happiness units by sitting next to George.
Bob would gain 87 happiness units by sitting next to Mallory.
Carol would lose 35 happiness units by sitting next to Alice.
Carol would lose 99 happiness units by sitting next to Bob.
Carol would lose 51 happiness units by sitting next to David.
Carol would gain 95 happiness units by sitting next to Eric.
Carol would gain 90 happiness units by sitting next to Frank.
Carol would lose 16 happiness units by sitting next to George.
Carol would gain 94 happiness units by sitting next to Mallory.
David would gain 36 happiness units by sitting next to Alice.
David would lose 18 happiness units by sitting next to Bob.
David would lose 65 happiness units by sitting next to Carol.
David would lose 18 happiness units by sitting next to Eric.
David would lose 22 happiness units by sitting next to Frank.
David would gain 2 happiness units by sitting next to George.
David would gain 42 happiness units by sitting next to Mallory.
Eric would lose 65 happiness units by sitting next to Alice.
Eric would gain 24 happiness units by sitting next to Bob.
Eric would gain 100 happiness units by sitting next to Carol.
Eric would gain 51 happiness units by sitting next to David.
Eric would gain 21 happiness units by sitting next to Frank.
Eric would gain 55 happiness units by sitting next to George.
Eric would lose 44 happiness units by sitting next to Mallory.
Frank would lose 48 happiness units by sitting next to Alice.
Frank would gain 91 happiness units by sitting next to Bob.
Frank would gain 8 happiness units by sitting next to Carol.
Frank would lose 66 happiness units by sitting next to David.
Frank would gain 97 happiness units by sitting next to Eric.
Frank would lose 9 happiness units by sitting next to George.
Frank would lose 92 happiness units by sitting next to Mallory.
George would lose 44 happiness units by sitting next to Alice.
George would lose 25 happiness units by sitting next to Bob.
George would gain 17 happiness units by sitting next to Carol.
George would gain 92 happiness units by sitting next to David.
George would lose 92 happiness units by sitting next to Eric.
George would gain 18 happiness units by sitting next to Frank.
George would gain 97 happiness units by sitting next to Mallory.
Mallory would gain 92 happiness units by sitting next to Alice.
Mallory would lose 96 happiness units by sitting next to Bob.
Mallory would lose 51 happiness units by sitting next to Carol.
Mallory would lose 81 happiness units by sitting next to David.
Mallory would gain 31 happiness units by sitting next to Eric.
Mallory would lose 73 happiness units by sitting next to Frank.
Mallory would lose 89 happiness units by sitting next to George.|]

preferences1 :: Preferences
preferences1 = fromListWith (<>) $ map parseLine $ lines input

parseLine :: String -> (Person, SeatmateQuality)
parseLine s =
  let s' = init s -- remove "." from end of line
      ws = words s'
      from = head ws
      to = last ws
      points = read (ws !! 3) :: Int
      sign = if "gain" `elem` ws then 1 else -1 -- "gain" is positive, "lose" negative
   in (from, fromList [(to, points * sign)])

neighbors :: [Person] -> [(Person, (Person, Person))]
neighbors ps =
  zipWith
    (\i p -> (p, (ps `index` (i - 1), ps `index` (i + 1))))
    [0 ..]
    ps

index :: [a] -> Int -> a
index xs n = xs !! (i `mod` length xs) where i = if n < 0 then length xs + n else n

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = map . map

happiness :: Preferences -> (Person, (Person, Person)) -> Int
happiness preferences (p, (pL, pR)) = (prefs ! pL) + (prefs ! pR) where prefs = preferences ! p

part1 :: IO ()
part1 = print $ maximum $ map sum $ map2 (happiness preferences1) $ map neighbors $ permutations $ keys preferences1

preferences2 :: Preferences
preferences2 =
  fromList $
    [("me", fromList $ map (\p -> (p, 0)) existingPeople)] ++ (map (second (insert "me" 0)) $ toList preferences1)
  where
    existingPeople = keys preferences1

part2 :: IO ()
part2 = print $ maximum $ map sum $ map2 (happiness preferences2) $ map neighbors $ permutations $ keys preferences2

run :: IO ()
run = part2

--run = part2
