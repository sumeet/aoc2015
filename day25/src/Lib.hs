module Lib (run) where

firstCode :: Integer
firstCode = 20151125

getFirstRow :: Int -> Int
getFirstRow colNo = sum [0 .. colNo]

getCell :: Int -> Int -> Int
getCell rowNo colNo = firstRow + sum (take (rowNo - 1) [colNo ..])
  where
    firstRow = getFirstRow colNo

nextCode :: Integer -> Integer
nextCode code = code * 252533 `rem` 33554393

run :: IO ()
run = print $ iterate nextCode firstCode !! (getCell 2981 3075 - 1)
