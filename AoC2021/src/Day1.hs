module Day1 where

countIncreasing :: [Int] -> Int
countIncreasing xs = length . filter (< 0) $ zipWith (-) xs (tail xs)

day1 :: String -> String
day1 = show . countIncreasing . map read . lines
