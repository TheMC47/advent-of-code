module Day1 where

import           Data.List

-- Part 1

-- O ( n log n ) by sorting the input and iterating
solve1 :: Int -> [Int] -> Int
solve1 target input = let (f, s) = go sorted (reverse sorted) in f * s
 where
  go :: [Int] -> [Int] -> (Int, Int)
  go (x : xs) (y : ys) | x + y == target = (x, y)
                       | x + y < target  = go xs (y : ys)
                       | otherwise       = go (x : xs) ys
  go _ _ = (0, 0)
  sorted = sort input

day1_1 :: IO ()
day1_1 = interact (show . solve1 2020 . map read . lines)


-- Part 2

solve2 :: [Int] -> Int
-- could be optimized by only reversing and sorting once, but I'm lazy
solve2 xs = head . filter (/= 0) $ map (\x -> x * solve1 (2020 - x) xs) xs

day1_2 :: IO ()
day1_2 = interact (show . solve2 . map read . lines)
