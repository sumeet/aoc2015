{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Data.List (subsequences)
import Text.RawString.QQ

sample :: IO ()
sample = print $ numCombsTo 25 [20, 15, 10, 5, 5]

part1 :: IO ()
part1 = print $ numCombsTo 150 nums where nums = map read $ lines input

part2 :: IO ()
part2 = print $ length $ filter (\c -> length c == minLengthOfComb) combs
  where
    minLengthOfComb = minimum $ map length combs
    combs = allCombs 150 (map read $ lines input)

run :: IO ()
run = part2

allCombs :: Int -> [Int] -> [[Int]]
allCombs total nums = filter (\ns -> sum ns == total) $ subsequences nums

numCombsTo :: Int -> [Int] -> Int
numCombsTo total nums = length $ allCombs total nums

input :: String
input =
  [r|50
44
11
49
42
46
18
32
26
40
21
7
18
43
10
47
36
24
22
40|]
