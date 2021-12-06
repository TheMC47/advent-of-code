module AoC2021.Day6 where

import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as M
import           Miloud

type Population = M.Map Int Integer

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

day6s :: [String -> String]
day6s = map day6 [80, 256]

day6 :: Int -> String -> String
day6 n =
    show
        . M.foldr (+) 0
        . flip (foldr (const tick)) [1 .. n]
        . foldr addFish M.empty
        . parseInput


tick :: Population -> Population
tick m = maybe m' (`hatch` m') hatched
  where
    (hatched, m') = pop (-1) $ M.mapKeys (subtract 1) m
    hatch n = updateDefault (+ n) n 8 . updateDefault (+ n) n 6

addFish :: Int -> Population -> Population
addFish = updateDefault (+ 1) 1
