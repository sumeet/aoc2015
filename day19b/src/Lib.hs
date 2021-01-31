{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Data.List (intercalate, isInfixOf, sortOn)
import Data.List.HT (search)
import Data.Maybe (catMaybes, listToMaybe)
import Debug.Trace (traceShowId)
import Text.RawString.QQ

parseSubstitution :: String -> (String, String)
parseSubstitution s = (head ws, last ws) where ws = words s

parseInput :: String -> ([(String, String)], String)
parseInput s = (substitutions, startingMolecule)
  where
    substitutions = sortOn (\(_, to) -> - (length to)) $ map parseSubstitution subLines
    subLines = (init . init) ls
    startingMolecule = last ls
    ls = lines s

replaceOne :: String -> String -> String -> String
replaceOne from to s = a ++ to ++ drop (length from) b
  where
    (a, b) = splitAt i s
    i = head $ search from s

goBackwardsTowardsEOld :: [(String, String)] -> String -> Int -> Int
goBackwardsTowardsEOld _ "e" numSubsSoFar = numSubsSoFar
goBackwardsTowardsEOld allSubs curString numSubsSoFar = goBackwardsTowardsEOld allSubs nextString (numSubsSoFar + 1)
  where
    nextString = replaceOne from to curString
    (to, from) = longestMatchingSub
    longestMatchingSub = head $ filter ((`isInfixOf` traceShowId curString) . snd) allSubs

goBackwardsTowardsE :: [(String, String)] -> String -> Int -> Maybe Int
goBackwardsTowardsE _ "e" numSubsSoFar = Just numSubsSoFar
goBackwardsTowardsE allSubs curString numSubsSoFar =
  listToMaybe $
    catMaybes $
      map
        ( \(to, from) ->
            let nextString = replaceOne from to curString in goBackwardsTowardsE allSubs nextString (numSubsSoFar + 1)
        )
        longestMatchingSubs
  where
    longestMatchingSubs = filter ((`isInfixOf` curString) . snd) allSubs

run :: IO ()
run = do
  print $ goBackwardsTowardsE allSubs startingMolecule 0
  where
    (allSubs, startingMolecule) = parseInput input

--run :: IO ()
--run = do
--  print $ length $ nub $ concatMap (\(from, to) -> replaceAllOnce from to startingMolecule) subs
--  where
--    subs = allSubs subMap
--    (subMap, startingMolecule) = parseInput input

sample :: String
sample =
  [r|e => H
e => O
H => HO
H => OH
O => HH

HOHOHO|]

input :: String
input =
  [r|Al => ThF
Al => ThRnFAr
B => BCa
B => TiB
B => TiRnFAr
Ca => CaCa
Ca => PB
Ca => PRnFAr
Ca => SiRnFYFAr
Ca => SiRnMgAr
Ca => SiTh
F => CaF
F => PMg
F => SiAl
H => CRnAlAr
H => CRnFYFYFAr
H => CRnFYMgAr
H => CRnMgYFAr
H => HCa
H => NRnFYFAr
H => NRnMgAr
H => NTh
H => OB
H => ORnFAr
Mg => BF
Mg => TiMg
N => CRnFAr
N => HSi
O => CRnFYFAr
O => CRnMgAr
O => HP
O => NRnFAr
O => OTi
P => CaP
P => PTi
P => SiRnFAr
Si => CaSi
Th => ThCa
Ti => BP
Ti => TiTi
e => HF
e => NAl
e => OMg

CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr|]
