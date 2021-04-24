{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Combinatorics (tuples)
import Text.RawString.QQ (r)

run :: IO ()
run = part2

part1 :: IO ()
part1 = print $ minimum $ map quantumEntanglement $ passengerCandidates input 3

part2 :: IO ()
part2 = print $ minimum $ map quantumEntanglement $ passengerCandidates input 4

quantumEntanglement :: [Int] -> Int
quantumEntanglement = product

passengerCandidates :: [Int] -> Int -> [[Int]]
passengerCandidates allPkgs numCompartments =
  head $
    filter (not . null) $
      map (\numPkgs -> filter hasCorrectWeight $ tuples numPkgs allPkgs) [1 ..]
  where
    hasCorrectWeight pkgs = sum pkgs == sum allPkgs `div` numCompartments

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
