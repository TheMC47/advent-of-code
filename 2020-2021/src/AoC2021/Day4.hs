{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module AoC2021.Day4 where

import           Data.List
import           Data.List.Split

type Board = [[(Int, Bool)]]

data Round = Round
    { lastScore       :: Int
    , remainingInput  :: [Int]
    , remainingBoards :: [Board]
    }

doRound :: Round -> Round
doRound Round {..} = Round newScore ns newBoards
  where
    n : ns    = remainingInput
    marked    = map (mark n) remainingBoards
    winner    = find valid marked
    newScore  = maybe lastScore (score n) winner
    newBoards = filter (not . valid) marked


empty :: Round -> Bool
empty Round {..} = null remainingInput || null remainingBoards


mark :: Int -> Board -> Board
mark n = map (map (\(m, f) -> (m, m == n || f)))

score :: Int -> Board -> Int
score = (. sumUnmarked) . (*)
  where
    sumUnmarked :: Board -> Int
    sumUnmarked = sum . map ((sum . map fst) . filter (not . snd))

valid :: Board -> Bool
valid b = valid' b || valid' (transpose b) where valid' = any (all snd)

initBoard :: [[Int]] -> Board
initBoard = map $ map (, False)

parseInput :: String -> Round
parseInput s = Round 0 input boards
  where
    input = map read . splitOn "," . head $ lines s
    boards =
        map (initBoard . map (map read . words))
            . tail
            . splitOn [""]
            . tail
            $ lines s


day4 :: (Round -> Bool) -> String -> String
day4 finished = show . lastScore . until finished doRound . parseInput

day4s :: [String -> String]
day4s = map day4 [(> 0) . lastScore, empty]
