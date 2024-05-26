{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Day1 as D1
import qualified Day10 as D10
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
import qualified Day7 as D7
import qualified Day8 as D8
import qualified Day9 as D9
import System.Environment
import System.Exit (exitFailure)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Read (readMaybe)

type Solver = String -> String

data Day = D {part1 :: !Solver, part2 :: !Solver, day :: !Int}

input :: Day -> String
input = ("input/day" <>) . show . day

type AOC = [Day]

mkAoc :: [(Solver, Solver)] -> AOC
mkAoc = zipWith (\i (p1, p2) -> D p1 p2 i) [1 ..]

aoc :: AOC
aoc =
    mkAoc
        [ (D1.part1, D1.part2)
        , (D2.part1, D2.part2)
        , (D3.part1, D3.part2)
        , (D4.part1, D4.part2)
        , (D5.part1, D5.part2)
        , (D6.part1, D6.part2)
        , (D7.part1, D7.part2)
        , (D8.part1, D8.part2)
        , (D9.part1, D9.part2)
        , (D10.part1, D10.part2)
        ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [readMaybe @Int -> Just dayNumber, readMaybe @Int -> Just part] ->
            if dayNumber > 0 && dayNumber <= length aoc
                then case part of
                    1 -> solve (dayNumber - 1) part1
                    2 -> solve (dayNumber - 1) part2
                    _ -> help
                else help
        _ -> help
  where
    help = putStrLn "Usage: aoc <day> <part>" >> exitFailure
    solve :: Int -> (Day -> Solver) -> IO ()
    solve d p = do
        let
            theDay = aoc !! d
            thePart = p theDay
            inputFile = input theDay
        contents <- hGetContents =<< openFile inputFile ReadMode
        putStrLn $ thePart contents
