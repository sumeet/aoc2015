{-# LANGUAGE LambdaCase #-}

module Lib where

import Data.Bimap (Bimap, fromList, size, (!), (!>))
import Data.List (nub, tails)

digits :: Bimap Char Int
digits = fromList $ zip ['a' .. 'z'] [0 ..]

numDigits :: Int
numDigits = size digits

toNum :: String -> Int
toNum [] = 0
--toNum [c] = digits ! c
toNum (c : cs) = (digits ! c * leftMostPlaceFactor) + toNum cs
  where
    leftMostPlaceFactor = numDigits ^ length cs

fromNum :: Int -> String
fromNum n | n < numDigits = [digits !> n]
fromNum n = fromNum (n `div` numDigits) ++ fromNum (n `mod` numDigits)

passwordIsValid :: String -> Bool
passwordIsValid s = all ($ s) [containsOneIncreasingStraight, all (`notElem` "iol"), containsTwoNonOverlappingPairs]

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq [_] = True
allEq (x : y : xs) = x == y && allEq (y : xs)

containsTwoNonOverlappingPairs :: String -> Bool
containsTwoNonOverlappingPairs s =
  let pairs = windows 2 (zip [0 ..] s)
      eqPairs = concat [[i, j] | [(i, c), (j, d)] <- pairs, c == d]
   in length (nub eqPairs) > 3

containsOneIncreasingStraight :: String -> Bool
containsOneIncreasingStraight s =
  any
    ( \case
        [x, y, z] -> toNum [z] - toNum [y] == 1 && toNum [y] - toNum [x] == 1
        _ -> error "unreachable"
    )
    $ windows 3 s

-- from https://stackoverflow.com/questions/27726739/implementing-an-efficient-sliding-window-algorithm-in-haskell#comment43882101_27733778
windows :: Int -> [a] -> [[a]]
windows n = foldr (zipWith (:)) (repeat []) . take n . tails

nextPass :: String -> String
nextPass s = if passwordIsValid s then s else nextPass (incrPass s)

incrPass :: String -> String
incrPass s = fromNum (toNum s + 1)

run :: IO ()
run = print $ nextPass "abcdefgh"
