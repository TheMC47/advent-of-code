module AoC2020.Day2 where

import Data.List.Split

day2 :: (Int -> Int -> Char -> String -> Bool) -> IO ()
day2 f = interact $ show . length . filter id . map (solve' f) . lines

-- Part 1

solve' :: (Int -> Int -> Char -> String -> Bool) -> String -> Bool
solve' policy xs =
  let [lower, higher, [c], _, rest] = splitOneOf "- :" xs
   in policy (read lower) (read higher) c rest

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

policy1 :: Int -> Int -> Char -> String -> Bool
policy1 lower higher c xs = let acc = count c xs in lower <= acc && acc <= higher

day2_1 :: IO ()
day2_1 = day2 policy1

-- Part2

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

policy2 :: Int -> Int -> Char -> String -> Bool
policy2 lower higher c xs = (xs !! (lower - 1) == c) `xor` (xs !! (higher - 1) == c)

day2_2 :: IO ()
day2_2 = day2 policy2
