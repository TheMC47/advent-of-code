{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day2 where

import Data.Char (isDigit)
import Data.Function
import Data.List
import Data.List.Split

data Game = G
    { gId :: !Int
    , pulls :: ![Pull]
    }
    deriving (Show)

data Pull = P {red :: !Int, green :: !Int, blue :: !Int}
    deriving (Show)

parseLine :: String -> Game
parseLine s | Just rest <- "Game " `stripPrefix` s = case splitOn ": " rest of
    [i, pulls] -> G (read i) . map parsePull $ splitOn "; " pulls

parsePull :: String -> Pull
parsePull s = foldl' (&) (P 0 0 0) (map parseColor (splitOn ", " s))
  where
    parseColor :: String -> Pull -> Pull
    parseColor s' p
        | "red" `isInfixOf` s' = p{red = n}
        | "blue" `isInfixOf` s' = p{blue = n}
        | "green" `isInfixOf` s' = p{green = n}
      where
        n = read $ filter isDigit s'

part1 :: String -> String
part1 = show . sum . map gId . filter validGame . map parseLine . lines

validGame :: Game -> Bool
validGame = all validPull . pulls

validPull :: Pull -> Bool
validPull P{..} = red <= 12 && green <= 13 && blue <= 14

part2 :: String -> String
part2 = show . sum . map (power . parseLine) . lines

power :: Game -> Int
power g = red p * green p * blue p
  where
    p = findMin g
    findMin :: Game -> Pull
    findMin G{pulls} = foldl' pullMax (P 0 0 0) pulls
    pullMax p' p'' =
        P
            { red = maxOn red p' p''
            , blue = maxOn blue p' p''
            , green = maxOn green p' p''
            }
    maxOn = (max `on`)
