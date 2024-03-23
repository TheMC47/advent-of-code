module Main where

import qualified Day1 as D1
import qualified Day2 as D2
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5

type Solver = String -> String

data Day = D {part1 :: !Solver, part2 :: !Solver, day :: !Int}

input :: Day -> String
input = ("input/day" <>) . show . day

data AOC = A
    { day1 :: !Day
    , day2 :: !Day
    , day3 :: !Day
    , day4 :: !Day
    , day5 :: !Day
    }

aoc :: AOC
aoc =
    A
        { day1 = D D1.part1 D1.part2 1
        , day2 = D D2.part1 D2.part2 2
        , day3 = D D3.part1 D3.part2 3
        , day4 = D D4.part1 D4.part2 4
        , day5 = D D5.part1 D5.part2 5
        }

main :: IO ()
main = solve day5 part2
  where
    solve d p = do
        let
            theDay = d aoc
            thePart = p theDay
            inputFile = input theDay
        contents <- hGetContents =<< openFile inputFile ReadMode
        putStrLn $ thePart contents
