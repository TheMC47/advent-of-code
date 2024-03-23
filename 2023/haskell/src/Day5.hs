{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Day5 where

import Control.Applicative
import Data.Foldable
import Data.Function
import Data.List.Split
import Data.Maybe

data Mapping = M {dest :: !Int, src :: !Int, len :: !Int}
    deriving (Show)

type Map = [Mapping]

data Almanac = A
    { seeds :: ![Int]
    , maps :: ![Map]
    }

tryMap :: Mapping -> Int -> Maybe Int
tryMap (M{..}) a
    | a >= src && diff < len = Just (dest + diff)
    | otherwise = Nothing
  where
    diff = a - src

mapTo :: Map -> Int -> Int
mapTo m a = fromMaybe a (asum (map (`tryMap` a) m))

seedToLocation :: Almanac -> Int -> Int
seedToLocation A{..} s = foldl' (&) s $ map mapTo maps

parse :: String -> Almanac
parse s =
    let (seedsLine : "" : rest) = lines s
        [ "seed-to-soil map:" : seedToSoil
            , "soil-to-fertilizer map:" : soilToFertilizer
            , "fertilizer-to-water map:" : fertilizerToWater
            , "water-to-light map:" : waterToLight
            , "light-to-temperature map:" : lightToTemperature
            , "temperature-to-humidity map:" : temperatureToHumidity
            , "humidity-to-location map:" : humidityToLocation
            ] = splitOn [""] rest
        ss = map read . drop 1 . words $ seedsLine
        convertLine :: String -> Mapping
        convertLine l =
            let [dest, src, len] = map read $ words l
             in M dest src len
        readSection :: [String] -> Map
        readSection = map convertLine
        ms = map readSection [seedToSoil, soilToFertilizer, fertilizerToWater, waterToLight, lightToTemperature, temperatureToHumidity, humidityToLocation]
     in A
            { seeds = ss
            , maps = ms
            }

seedLocations :: Almanac -> [Int]
seedLocations a = map (seedToLocation a) (seeds a)

part1 :: String -> String
part1 = show . minimum . map toInteger . seedLocations . parse

part2 :: String -> String
part2 = show . minimum . map (toInteger . fst) . filter ((> 0) . snd) . seedLocations2 . parse2

type Range = (Int, Int)

data Almanac2 = A2
    { seeds2 :: ![Range]
    , maps2 :: ![Map]
    }

parse2 :: String -> Almanac2
parse2 s =
    let (seedsLine : "" : rest) = lines s
        [ "seed-to-soil map:" : seedToSoil
            , "soil-to-fertilizer map:" : soilToFertilizer
            , "fertilizer-to-water map:" : fertilizerToWater
            , "water-to-light map:" : waterToLight
            , "light-to-temperature map:" : lightToTemperature
            , "temperature-to-humidity map:" : temperatureToHumidity
            , "humidity-to-location map:" : humidityToLocation
            ] = splitOn [""] rest
        ss = group2 . map read . drop 1 . words $ seedsLine
        group2 :: [Int] -> [Range]
        group2 [] = []
        group2 (a : b : xs) = (a, b) : group2 xs
        group2 _ = error "group2: odd number of elements"
        convertLine :: String -> Mapping
        convertLine l =
            let [dest, src, len] = map read $ words l
             in M dest src len
        readSection :: [String] -> Map
        readSection = map convertLine
        ms = map readSection [seedToSoil, soilToFertilizer, fertilizerToWater, waterToLight, lightToTemperature, temperatureToHumidity, humidityToLocation]
     in A2
            { seeds2 = ss
            , maps2 = ms
            }

mapRange :: Range -> Mapping -> ([Range], [Range])
mapRange (start, len) (M dest srcStart lenMap) =
    let end = start + len
        srcEnd = srcStart + lenMap
     in if
            | start < srcStart ->
                if
                    | end <= srcStart -> ([], [(start, len)])
                    | end <= srcEnd -> ([(dest, end - srcStart)], [(start, srcStart - start)])
                    | otherwise -> ([(dest, lenMap)], [(start, srcStart - start), (srcEnd, end - srcEnd)])
            | start >= srcStart ->
                if
                    | end <= srcEnd -> ([(dest + start - srcStart, len)], [])
                    | start < srcEnd -> ([(dest + start - srcStart, srcEnd - start)], [(srcEnd, end - srcEnd)])
                    | otherwise -> ([], [(start, len)])

rangeMapping :: Range -> Map -> [Range]
rangeMapping r theMap = go theMap ([], [r])
  where
    go [] (inside, outside) = inside ++ outside
    go (m : ms) (inside, outside) =
        let results = map (`mapRange` m) outside
            insides = concatMap fst results
            outsides = concatMap snd results
         in go ms (inside ++ insides, outsides)

seedLocations2 :: Almanac2 -> [Range]
seedLocations2 (A2 seeds mappings) = go mappings seeds
  where
    go :: [Map] -> [Range] -> [Range]
    go [] rs = rs
    go (m : ms) rs = go ms (concatMap (`rangeMapping` m) rs)
