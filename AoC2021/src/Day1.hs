module Day1 where

import System.IO

countIncreasing :: [Int] -> Int
countIncreasing xs = length . filter (< 0) $ zipWith (-) xs (tail xs)

solveDay1 :: String -> String
solveDay1 = show . countIncreasing . map read . lines

day1 :: IO ()
day1 = do
  handle <- openFile "inputs/day1" ReadMode
  contents <- hGetContents handle
  print $ solveDay1 contents
