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

solve :: String -> (Int, Int)
solve = solve' []
  where
    solve' [] []                      = (0, 0)
    solve' ys []                      = (0, calcPointsIncomplete ys 0)
    solve' ys (c : cs) | open c       = solve' (close c : ys) cs
    solve' (y : ys) (c : cs) | y == c = solve' ys cs
    solve' _ (c : cs)                 = (pointsCorrupted c, 0)

    calcPointsIncomplete [] n = n
    calcPointsIncomplete (y : ys) n =
        calcPointsIncomplete ys (5 * n + pointsIncomplete y)

    pointsCorrupted ')' = 3
    pointsCorrupted ']' = 57
    pointsCorrupted '}' = 1197
    pointsCorrupted '>' = 25137

    pointsIncomplete ')' = 1
    pointsIncomplete ']' = 2
    pointsIncomplete '}' = 3
    pointsIncomplete '>' = 4

day10_1 :: String -> String
day10_1 = show . sum . map (fst . solve) . lines

day10_2 :: String -> String
day10_2 = show . median . filter (0 /=) . map (snd . solve) . lines
