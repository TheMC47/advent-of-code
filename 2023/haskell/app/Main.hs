module Main where

import qualified Day1 as D1
import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Solver = String -> String

data Day = D {part1 :: !Solver, part2 :: !Solver, day :: !Int}

input :: Day -> String
input = ("input/day" <>) . show . day

data AOC = A
    { day1 :: !Day
    }

aoc :: AOC
aoc =
    A
        { day1 = D D1.part1 D1.part2 1
        }

main :: IO ()
main = solve day1 part1
  where
    solve d p = do
        let
            theDay = d aoc
            thePart = p theDay
            inputFile = input theDay
        contents <- hGetContents =<< openFile inputFile ReadMode
        putStrLn $ thePart contents
