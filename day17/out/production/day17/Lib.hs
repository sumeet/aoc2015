{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Data.List (subsequences)
import Text.RawString.QQ

run :: IO ()
run = print $ numCombsTo 150 nums where nums = map read $ lines input

sample :: IO ()
sample = print $ numCombsTo 25 [20, 15, 10, 5, 5]

numCombsTo :: Int -> [Int] -> Int
numCombsTo total nums = length $ filter (\ns -> sum ns == total) $ subsequences nums

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
