{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Data.List (isInfixOf, sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Internal.Search (indices)
import Text.RawString.QQ

parseSubstitution :: Text -> (Text, Text)
parseSubstitution s = (head ws, last ws) where ws = T.words s

parseInput :: Text -> ([(Text, Text)], Text)
parseInput s = (substitutions, startingMolecule)
  where
    substitutions = sortOn (\(_, to) -> - (T.length to)) $ map parseSubstitution subLines
    subLines = (init . init) ls
    startingMolecule = last ls
    ls = T.lines s

replaceOne :: Text -> Text -> Text -> Text
replaceOne from to s = a `T.append` to `T.append` T.drop (T.length from) b
  where
    (a, b) = T.splitAt i s
    i = head $ indices from s

goBackwardsTowardsE :: [(Text, Text)] -> Text -> Int -> Maybe Int
goBackwardsTowardsE _ "e" numSubsSoFar = Just numSubsSoFar
goBackwardsTowardsE allSubs curString numSubsSoFar =
  listToMaybe $
    mapMaybe
      ( \(to, from) ->
          let nextString = replaceOne from to curString
           in goBackwardsTowardsE allSubs nextString (numSubsSoFar + 1)
      )
      longestMatchingSubs
  where
    longestMatchingSubs = filter ((`T.isInfixOf` curString) . snd) allSubs

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
