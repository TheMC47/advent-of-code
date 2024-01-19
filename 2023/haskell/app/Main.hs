module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import qualified Day1 as D1

main :: IO ()
main = do
    contents <- hGetContents =<< openFile "input/day1" ReadMode
    putStrLn $ D1.part2 contents
