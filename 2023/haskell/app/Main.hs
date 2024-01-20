module Main where

import qualified Day1 as D1
import qualified Day2 as D2
import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Solver = String -> String

data Day = D {part1 :: !Solver, part2 :: !Solver, day :: !Int}

input :: Day -> String
input = ("input/day" <>) . show . day

data AOC = A
    { day1 :: !Day
    , day2 :: !Day
    }

aoc :: AOC
aoc =
    A
        { day1 = D D1.part1 D1.part2 1
        , day2 = D D2.part1 D2.part2 2
        }

main :: IO ()
main = solve day2 part2
  where
    solve d p = do
        let
            theDay = d aoc
            thePart = p theDay
            inputFile = input theDay
        contents <- hGetContents =<< openFile inputFile ReadMode
        putStrLn $ thePart contents
