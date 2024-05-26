module Day9 where

type Input = [[Integer]]

parse :: String -> Input
parse = map (map read . words) . lines

finished :: [Integer] -> Bool
finished = all (== 0)

step :: [Integer] -> [Integer]
step [] = []
step [_] = []
step (x : y : xs) = (y - x) : step (y : xs)

steps :: [Integer] -> [[Integer]]
steps xs
    | finished xs = [xs]
    | otherwise = xs : steps (step xs)

extrapolateForward :: [[Integer]] -> Integer
extrapolateForward = sum . map last

extrapolateBackward :: [[Integer]] -> Integer
extrapolateBackward ss =
    let firsts = reverse $ map head ss
        go acc [] = acc
        go acc (x : xs) = go (x - acc) xs
     in go 0 firsts

continueSequneceWith :: ([[Integer]] -> Integer) -> [Integer] -> Integer
continueSequneceWith = (. steps)

solve :: ([[Integer]] -> Integer) -> String -> String
solve f = show . sum . map (continueSequneceWith f) . parse

part1 :: String -> String
part1 = solve extrapolateForward

part2 :: String -> String
part2 = solve extrapolateBackward
