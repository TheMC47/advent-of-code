{-# LANGUAGE ViewPatterns #-}

module AoC2021.Day2 where

import Miloud

day2_1 :: String -> String
day2_1 = day2 (uncurry (*)) go (0, 0)
  where
    go :: (Int, Int) -> [String] -> (Int, Int)
    go (h, v) [dir, read -> n]
      | dir == "forward" = (h + n, v)
      | dir == "up" = (h, v - n)
      | dir == "down" = (h, v + n)
    go _ _ = undefined

day2_2 :: String -> String
day2_2 = day2 (uncurry3 (const .: (*))) go (0, 0, 0)
  where
    go :: (Int, Int, Int) -> [String] -> (Int, Int, Int)
    go (h, v, a) [dir, read -> n]
      | dir == "forward" = (h + n, v + a * n, a)
      | dir == "up" = (h, v, a - n)
      | dir == "down" = (h, v, a + n)
    go _ _ = undefined

day2 :: (b -> Int) -> (b -> [String] -> b) -> b -> String -> String
day2 aggregator folder z = show . aggregator . foldl folder z . map words . lines
