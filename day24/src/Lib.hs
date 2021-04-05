{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Text.RawString.QQ (r)

run :: IO ()
run = print input

sample :: [Int]
sample = [1, 2, 3, 4, 5, 7, 11]

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
