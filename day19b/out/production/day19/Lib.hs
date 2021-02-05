{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Data.Graph.AStar (aStar)
import qualified Data.HashSet as HS
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Internal.Search (indices)
--import System.Random.S
import Text.RawString.QQ

neighbors :: [(Text, Text)] -> Text -> HS.HashSet Text
neighbors allSubs curString =
  HS.fromList $
    concatMap (\(to, from) -> replaceAllOnce from to curString) allSubs

parseSubstitution :: Text -> (Text, Text)
parseSubstitution s = (head ws, last ws) where ws = T.words s

parseInput :: Text -> ([(Text, Text)], Text)
parseInput s = (substitutions, startingMolecule)
  where
    substitutions = sortOn (\(_, to) -> - (T.length to)) $ map parseSubstitution subLines
    subLines = (init . init) ls
    startingMolecule = last ls
    ls = T.lines s

replaceAllOnce :: Text -> Text -> Text -> [Text]
replaceAllOnce from to string =
  [a `T.append` to `T.append` T.drop subLen b | (a, b) <- map (`T.splitAt` string) matchIndices]
  where
    subLen = T.length from
    matchIndices = indices from string

run :: IO ()
run = do
  print $ aStar (neighbors allSubs) (\_ _ -> 1) T.length ("e" ==) startingMolecule
  where
    (allSubs, startingMolecule) = parseInput input

sample :: Text
sample =
  [r|e => H
e => O
H => HO
H => OH
O => HH

HOHOHO|]

input :: Text
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
