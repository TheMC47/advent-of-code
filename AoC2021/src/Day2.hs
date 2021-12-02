{-# LANGUAGE ViewPatterns #-}

module Day2 where

import Miloud

day2 :: String -> String
day2 = day2_2

day2_1 :: String -> String
day2_1 = show . uncurry (*) . foldl go (0, 0) . map words . lines
  where
    go :: (Int, Int) -> [String] -> (Int, Int)
    go (h, v) [dir, read -> n]
      | dir == "forward" = (h + n, v)
      | dir == "up" = (h, v - n)
      | dir == "down" = (h, v + n)
    go _ _ = undefined

day2_2 :: String -> String
day2_2 =
  show . uncurry3 (const .: (*))
    . foldl go (0, 0, 0)
    . map words
    . lines
  where
    go :: (Int, Int, Int) -> [String] -> (Int, Int, Int)
    go (h, v, a) [dir, read -> n]
      | dir == "forward" = (h + n, v + a * n, a)
      | dir == "up" = (h, v, a - n)
      | dir == "down" = (h, v, a + n)
    go _ _ = undefined
