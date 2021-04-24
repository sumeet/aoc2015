{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Combinatorics (variate)
import Text.RawString.QQ (r)

run :: IO ()
run = print $ minimum $ map quantumEntanglement $ passengerCandidates input

quantumEntanglement :: [Int] -> Int
quantumEntanglement = product

passengerCandidates :: [Int] -> [[Int]]
passengerCandidates allPkgs =
  head $
    filter (not . null) $
      map (\numPkgs -> filter hasCorrectWeight $ variate numPkgs allPkgs) [1 ..]
  where
    hasCorrectWeight pkgs = sum pkgs == sum allPkgs `div` 3

sample :: [Int]
sample = [1 .. 5] ++ [7 .. 11]

input :: [Int]
input =
  map read $
    lines
      [r|1
3
5
11
13
17
19
23
29
31
41
43
47
53
59
61
67
71
73
79
83
89
97
101
103
107
109
113|]
