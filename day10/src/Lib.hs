module Lib where

import Data.Function ((&))
import qualified Data.List as List

sample :: String
sample = "111221"

input :: String
input = "1321131112"

lookAndSay :: String -> String
lookAndSay s = map (\group -> show (length group) ++ [head group]) (List.group s) & concat

part1 :: IO ()
part1 = print $ length $ iterate lookAndSay input !! 40

part2 :: IO ()
part2 = print $ length $ iterate lookAndSay input !! 50

run :: IO ()
run = part2
