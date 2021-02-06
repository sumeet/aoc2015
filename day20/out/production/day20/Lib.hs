module Lib where

import Control.Parallel.Strategies (parBuffer, rdeepseq, runEval)
import Data.List (find)

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n `div` 2] ++ [n], n `mod` x == 0]

numPresentsFor :: Int -> Int
numPresentsFor = (* 10) . sum . factors

factors2 :: Int -> [Int]
factors2 n = filter (\f -> n <= f * 50) $ factors n

numPresentsFor2 :: Int -> Int
numPresentsFor2 = (* 11) . sum . factors2

part1 :: IO ()
part1 = print $ find ((>= input) . fst) presentCountsPar
  where
    presentCountsPar = runEval $ parBuffer 32 rdeepseq presentCounts
    presentCounts = [(numPresentsFor n, n) | n <- [start ..]]
    start = 1

part2 :: IO ()
part2 = print $ find ((>= input) . fst) presentCountsPar
  where
    presentCountsPar = runEval $ parBuffer 32 rdeepseq presentCounts
    presentCounts = [(numPresentsFor2 n, n) | n <- [start ..]]
    start = 1

run :: IO ()
run = part2

input :: Int
input = 34000000
