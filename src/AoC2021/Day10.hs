module AoC2021.Day10 where


import           Data.List
import           Miloud
open :: Char -> Bool
open = (`elem` "([<{")

close :: Char -> Char
close '(' = ')'
close '[' = ']'
close '{' = '}'
close '<' = '>'

solveCorrupted :: String -> Int
solveCorrupted = solve' []
  where
    solve' _ []                       = 0
    solve' ys (c : cs) | open c       = solve' (close c : ys) cs
    solve' (y : ys) (c : cs) | y == c = solve' ys cs
    solve' _ (c : cs)                 = points c
    points :: Char -> Int
    points ')' = 3
    points ']' = 57
    points '}' = 1197
    points '>' = 25137

solveIncomplete :: String -> Int
solveIncomplete = solve' []
  where
    solve' [] []                      = 0
    solve' ys []                      = calcPoints ys 0
    solve' ys (c : cs) | open c       = solve' (close c : ys) cs
    solve' (y : ys) (c : cs) | y == c = solve' ys cs
    solve' _ (c : cs)                 = 0

    calcPoints []       n = n
    calcPoints (y : ys) n = calcPoints ys (5 * n + points y)

    points :: Char -> Int
    points ')' = 1
    points ']' = 2
    points '}' = 3
    points '>' = 4

day10_1 :: String -> String
day10_1 = show . sum . map solveCorrupted . lines


day10_2 :: String -> String
day10_2 = show . median . filter (0 /=) . map solveIncomplete . lines
