{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Data.Bifunctor (second)
import Data.List (nub)
import Data.List.HT (search)
import Data.Map (Map, fromListWith, toList)
import Text.RawString.QQ

parseSubstitution :: String -> (String, String)
parseSubstitution s = (head ws, last ws) where ws = words s

allSubs :: Map String [String] -> [(String, String)]
allSubs subs = concatMap (\(from, tos) -> map (from,) tos) $ toList subs

parseInput :: String -> (Map String [String], String)
parseInput s = (substitutions, startingMolecule)
  where
    substitutions = fromListWith (++) $ map (second (: []) . parseSubstitution) subLines
    subLines = (init . init) ls
    startingMolecule = last ls
    ls = lines s

replaceAllOnce :: String -> String -> String -> [String]
replaceAllOnce from to string =
  [a ++ to ++ drop subLen b | (a, b) <- map (`splitAt` string) matchIndices]
  where
    subLen = length from
    matchIndices = search from string

run :: IO ()
run = do
  print $ length $ nub $ concatMap (\(from, to) -> replaceAllOnce from to startingMolecule) subs
  where
    subs = allSubs subMap
    (subMap, startingMolecule) = parseInput input

sample :: String
sample =
  [r|H => OO

H2O|]

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
