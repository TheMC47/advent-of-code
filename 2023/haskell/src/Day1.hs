{-# LANGUAGE TypeApplications #-}

module Day1 (part1, part2) where

import Data.Char (isDigit)
import Data.List

day1 :: (String -> Int) -> String -> String
day1 hl = show . sum . map hl . lines

part1 :: String -> String
part1 = day1 handleLine
  where
    handleLine :: String -> Int
    handleLine s = f * 10 + l
      where
        go = read @Int . take 1 . filter isDigit
        f = go s
        l = go . reverse $ s

part2 :: String -> String
part2 = day1 handleLine
  where
    handleLine :: String -> Int
    handleLine s = f * 10 + l
      where
        l = findLast s
        f = findFirst s validNumbers
        findFirst :: String -> [String] -> Int
        findFirst s'@(x : xs) checks
            | isDigit x = read [x]
            | otherwise = case findIndex (`isPrefixOf` s') checks of
                Just i -> i + 1
                Nothing -> findFirst xs checks
        findFirst _ _ = 0
        findLast s' = findFirst (reverse s') $ map reverse validNumbers
        validNumbers =
            [ "one"
            , "two"
            , "three"
            , "four"
            , "five"
            , "six"
            , "seven"
            , "eight"
            , "nine"
            ]
