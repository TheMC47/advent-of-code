module Day7 (main) where

import Data.List.Split (splitOn)
import Utils

data Equation = E {r :: !Integer, ops :: ![Integer]}

isValid :: Equation -> Bool
isValid (E result operands) =
    case operands of
        [] -> False
        [x] -> result == x
        (x : y : xs) ->
            (x <= result) && (isValid (E result (x + y : xs)) || isValid (E result (x * y : xs)))

part1 :: [Equation] -> Integer
part1 = sum . map r . filter isValid

parse :: String -> [Equation]
parse s = map parseEq (lines s)
  where
    parseEq :: String -> Equation
    parseEq l =
        let [parsedR, rest] = splitOn ": " l
         in E (read parsedR) (map read . words $ rest)

concatI :: Integer -> Integer -> Integer
concatI a b
    | b < 10 = a * 10 + b
    | b < 100 = a * 100 + b
    | b < 1000 = a * 1000 + b
    | otherwise = error "NOOO"

isValid2 :: Equation -> Bool
isValid2 (E result operands) =
    case operands of
        [] -> False
        [x] -> result == x
        (x : y : xs) ->
            (x <= result)
                && ( isValid2 (E result (x + y : xs))
                        || isValid2 (E result (x * y : xs))
                        || isValid2 (E result (concatI x y : xs))
                   )

part2 :: [Equation] -> Integer
part2 = sum . map r . filter isValid2

main :: IO ()
main = do
    contents <- parse <$> input 7
    putStrLn $ "Part1: " <> show (part1 contents)
    putStrLn $ "Part2: " <> show (part2 contents)
