module Day1 where

import           Data.List

 -- O ( n log n ) by sorting the input and iterating
solve :: [Int] -> Int
solve input = let (f, s) = go 2020 sorted (reverse sorted) in f * s
 where
  go :: Int -> [Int] -> [Int] -> (Int, Int)
  go target (x : xs) (y : ys) | x + y == target = (x, y)
                              | x + y < target  = go target xs (y : ys) -- up
                              | otherwise       = go target (x : xs) ys -- down
  go _ _ _ = (0, 0)
  sorted = sort input

day1 :: IO ()
day1 = interact (show . solve . map read . lines)
