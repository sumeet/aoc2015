module Lib where

import Data.Function ((&))
import qualified Data.List as List

sample :: String
sample = "111221"

input :: String
input = "1321131112"

lookAndSay :: String -> String
lookAndSay s = map (\group -> show (length group) ++ [head group]) (List.group s) & concat

run :: IO ()
run = print $ length $ iterate lookAndSay input !! 40
