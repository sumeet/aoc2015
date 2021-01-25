{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Data.List (permutations)
import Data.Map (Map, fromListWith, keys)
import Text.RawString.QQ

type Person = String

data SeatmateQuality = SeatmateQuality Person Int deriving (Show)

type Preferences = Map Person [SeatmateQuality]

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

preferences :: Preferences
preferences = fromListWith (<>) $ map parseLine $ lines sample

parseLine :: String -> (Person, [SeatmateQuality])
parseLine s =
  let s' = init s -- remove "." from end of line
      ws = words s'
      from = head ws
      to = last ws
      points = read (ws !! 3) :: Int
      sign = if "gain" `elem` ws then 1 else -1 -- "gain" is positive, "lose" negative
   in (from, [SeatmateQuality to (points * sign)])

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

run :: IO ()
run = print $ maximum $ map sum $ map2 (\(p, (pL, pR)) -> 123) $ map neighbors $ permutations $ keys preferences
