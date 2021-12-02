module AoC2021.Day1 where

import Data.List.Split


day1 :: ([Int] -> Int) -> String -> String
day1 f = show . f . map read . lines

countIncreasing :: [Int] -> Int
countIncreasing xs = length . filter (< 0) $ zipWith (-) xs (tail xs)

day1_1 :: String -> String
day1_1 = day1 countIncreasing


countIncreasingSlidingWindow :: [Int] -> Int
countIncreasingSlidingWindow xs = countIncreasing
    $ zipWith3 (\x y z -> x + y + z) xs (tail xs) (tail (tail xs))

day1_2 :: String -> String
day1_2 = day1 countIncreasingSlidingWindow
