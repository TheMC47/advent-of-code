{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day4 where

import Data.Foldable
import Data.List.Split (splitOn)
import qualified Data.Map as M

data Card = C
    { winning :: ![Int]
    , mine :: ![Int]
    }
    deriving (Show)

parseLine :: String -> Card
parseLine s = case splitOn ": " s of
    [_, nums] -> case splitOn "|" nums of
        [w, m] -> C (map read $ words w) (map read $ words m)

countWinning :: Card -> Int
countWinning (C w m) = length $ filter (`elem` w) m

points :: Card -> Int
points c =
    let n = countWinning c
     in if n == 0 then 0 else 2 ^ (n - 1)

part1 :: String -> String
part1 = show . sum . map (points . parseLine) . lines

type Copies = M.Map Int Int
type Game = [Card]

parseInput :: String -> Game
parseInput = map parseLine . lines

play :: Game -> Int
play g = play' m g 1
  where
    m = M.fromList $ map (,1) [1 .. length g]
    play' copies (c : cs) idx =
        let
            n = countWinning c
            extra = [idx + 1 .. idx + n]
            currentOccurences = copies M.! idx
            copies' = foldl' (\m' i -> M.insertWith (+) i currentOccurences m') copies extra
         in
            play' copies' cs (idx + 1)
    play' copies [] _ = sum $ M.elems copies

part2 :: String -> String
part2 = show . play . parseInput
