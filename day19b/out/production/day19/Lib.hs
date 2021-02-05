{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Arrow (second)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Internal.Search (indices)
import System.Random (RandomGen, mkStdGen)
import System.Random.Shuffle (shuffle')
import Text.RawString.QQ

-- wow... https://stackoverflow.com/a/23924238/149987
converge :: Eq a => (a -> a) -> a -> a
converge f x = let y = f x in if x == y then x else converge f y

replaceWithCount :: Text -> Text -> Text -> (Text, Int)
replaceWithCount t from to = (T.replace from to t, length $ indices from t)

tryShuffle :: [(Text, Text)] -> Text -> (Text, Int)
tryShuffle allSubs curString = converge tryApplyAllSubs (curString, 0)
  where
    tryApplyAllSubs (s, subCount) =
      foldl
        ( \(s', subCount') (to, from) ->
            second (subCount' +) $ replaceWithCount s' from to
        )
        (s, subCount)
        allSubs

shuf :: RandomGen gen => [a] -> gen -> [a]
shuf xs = shuffle' xs $ length xs

run :: IO ()
run = do
  print $ find (("e" ==) . fst) $ map ((`tryShuffle` startingMolecule) . shuf allSubs . mkStdGen) [0 ..]
  where
    (allSubs, startingMolecule) = parseInput input

parseSubstitution :: Text -> (Text, Text)
parseSubstitution s = (head ws, last ws) where ws = T.words s

parseInput :: Text -> ([(Text, Text)], Text)
parseInput s = (substitutions, startingMolecule)
  where
    substitutions = map parseSubstitution subLines
    subLines = (init . init) ls
    startingMolecule = last ls
    ls = T.lines s

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
