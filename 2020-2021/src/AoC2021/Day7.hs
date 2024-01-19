module AoC2021.Day7 where


import           Control.Monad
import           Data.List.Split

day7s :: [String -> String]
day7s = map day7 [id, \x -> x * (x + 1) `div` 2]


alignAt :: (Int -> Int) -> Int -> [Int] -> Int
alignAt f n = sum . map (f . abs . subtract n)


solve :: (Int -> Int) -> [Int] -> Int
solve f xs =
    minimum . map (flip (alignAt f) xs) $ liftM2 enumFromTo minimum maximum xs


day7 :: (Int -> Int) -> String -> String
day7 f = show . solve f . map read . splitOn ","


alignAt' :: Int -> [Int] -> Int
alignAt' n = sum . map (abs . subtract n)

solve' :: ([Int] -> Int) -> [Int] -> Int
solve' f = liftM2 alignAt' f id
