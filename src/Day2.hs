module Day2 where

import           Data.List.Split

solve :: (String -> Bool) -> [String] -> Int
solve f = length . filter id . map f

day2 :: (String -> Bool) -> IO ()
day2 f = interact $ show . solve f . lines

-- Part 1

solve1 :: String -> Bool
solve1 xs =
  let [lower, higher, [c], _, rest] = splitOneOf "- :" xs
  in  policy1 (read lower) (read higher) c rest

policy1 :: Int -> Int -> Char -> String -> Bool
policy1 lower higher c = go 0
 where
  go acc [] = lower <= acc && acc <= higher
  go acc (x : xs) | acc > higher = False
                  | c == x       = go (acc + 1) xs
                  | otherwise    = go acc xs

day2_1 :: IO ()
day2_1 = day2 solve1

-- Part2

xor :: Bool -> Bool -> Bool
xor True  False = True
xor False True  = True
xor _     _     = False

policy2 :: Int -> Int -> Char -> String -> Bool
policy2 lower higher c xs = (xs !! (lower - 1) == c) `xor` (xs !! (higher - 1) == c)

solve2 :: String -> Bool
solve2 xs =
  let [lower, higher, [c], _, rest] = splitOneOf "- :" xs
  in  policy2 (read lower) (read higher) c rest


day2_2 :: IO ()
day2_2 = day2 solve2
