{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Combinatorics (variate, variateRep)
import Data.List (transpose)
import Debug.Trace (traceShowId)
import Text.RawString.QQ

sample :: String
sample =
  [r|Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3|]

input :: String
input =
  [r|Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5
PeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1
Frosting: capacity 0, durability -1, flavor 4, texture 0, calories 6
Sugar: capacity -1, durability 0, flavor 0, texture 2, calories 8|]

parseLine :: String -> ([Int], Int)
parseLine s = (stats, calories)
  where
    stats = map (read . (ws !!)) [2, 4, 6, 8]
    calories = read $ ws !! 10
    ws = words $ filter (/= ',') s

allocations :: Int -> Int -> [[Int]]
allocations slots total = filter (\s -> sum s == total) $ variateRep slots [0 .. total]

calculateCookies1 :: [[Int]] -> [Int] -> Int
calculateCookies1 statss allocation =
  let statsByPosition = transpose statss
   in product $
        map
          (\statsForThisPos -> max 0 $ sum $ zipWith (*) statsForThisPos allocation)
          statsByPosition

optimize1 :: [[Int]] -> Int -> Int
optimize1 statss total =
  let allocs = allocations (length statss) total
   in maximum $ map (calculateCookies1 statss) allocs

part1 :: IO ()
part1 = print $ optimize1 statss 100 where statss = map (fst . parseLine) $ lines input

calculateCookies2 :: [([Int], Int)] -> [Int] -> Int
calculateCookies2 statss allocation = if totalCalories == 500 then product cookieStats else 0
  where
    cookieStats = init cookieProperties
    totalCalories = last cookieProperties
    cookieProperties =
      map
        (\statsForThisPos -> max 0 $ sum $ zipWith (*) statsForThisPos allocation)
        statsByPosition
    statsByPosition = transpose $ map (\(stats, calories) -> stats ++ [calories]) statss

optimize2 :: [([Int], Int)] -> Int -> Int
optimize2 statss total =
  let allocs = allocations (length statss) total
   in maximum $ map (calculateCookies2 statss) allocs

part2 :: IO ()
part2 = print $ optimize2 statss 100 where statss = map parseLine $ lines input

run :: IO ()
run = part2
