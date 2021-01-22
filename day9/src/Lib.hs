{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Monad
import Data.List ((\\))
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import Debug.Trace (trace)
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.RawString.QQ

sample :: String
sample =
  [r|London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141|]

input :: String
input =
  [r|Tristram to AlphaCentauri = 34
Tristram to Snowdin = 100
Tristram to Tambi = 63
Tristram to Faerun = 108
Tristram to Norrath = 111
Tristram to Straylight = 89
Tristram to Arbre = 132
AlphaCentauri to Snowdin = 4
AlphaCentauri to Tambi = 79
AlphaCentauri to Faerun = 44
AlphaCentauri to Norrath = 147
AlphaCentauri to Straylight = 133
AlphaCentauri to Arbre = 74
Snowdin to Tambi = 105
Snowdin to Faerun = 95
Snowdin to Norrath = 48
Snowdin to Straylight = 88
Snowdin to Arbre = 7
Tambi to Faerun = 68
Tambi to Norrath = 134
Tambi to Straylight = 107
Tambi to Arbre = 40
Faerun to Norrath = 11
Faerun to Straylight = 66
Faerun to Arbre = 144
Norrath to Straylight = 115
Norrath to Arbre = 135
Straylight to Arbre = 127|]

regularParse :: Parser s -> String -> Either ParseError s
regularParse input =
  let filename = ""
   in parse input filename

data Edge = Edge
  { src :: String,
    dest :: String,
    distance :: Int
  }
  deriving (Show)

-- example: "London to Dublin = 464"
parseEdge :: Parser Edge
parseEdge = do
  src <- many $ noneOf " "
  _ <- string " to "
  dest <- many $ noneOf " "
  _ <- string " = "
  distance <- liftM read $ many1 digit
  return $ Edge {src, dest, distance}

tryE :: Show l => Either l r -> r
tryE (Right right) = right
tryE (Left l) = error $ show l

edges :: [Edge]
edges = tryE $ sequence $ map (regularParse parseEdge) (lines input)

destsBySrc :: Map.Map String [(String, Int)]
destsBySrc =
  Map.fromListWith (<>) $
    concat $
      map
        ( \edge ->
            [ ((src edge), [(dest edge, distance edge)]),
              ((dest edge), [(src edge, distance edge)])
            ]
        )
        edges

data Route = Route
  { visited :: [String],
    total :: Int
  }
  deriving (Show)

allRoutesFrom :: Route -> [Route]
allRoutesFrom Route {visited, total} =
  let nextDests = case visited of
        [] -> map (\dest -> (dest, 0)) $ Map.keys destsBySrc
        _ -> filter (\(dest, _) -> not $ dest `elem` visited) (destsBySrc ! last visited)
      nextRoutes = map (\(dest, cost) -> Route {visited = visited ++ [dest], total = total + cost}) nextDests
      (ongoingRoutes, endedRoutes) = List.partition (\Route {visited, ..} -> length visited < length destsBySrc) nextRoutes
   in endedRoutes ++ (concat $ map allRoutesFrom ongoingRoutes)

run :: IO ()
run = putStrLn $ show $ minimum $ map total $ allRoutesFrom Route {visited = [], total = 0}
