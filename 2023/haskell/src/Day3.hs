{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day3 where

import Control.Monad (guard)
import Data.Char (isDigit, ord)
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import Data.List

type Coord = (Int, Int)

data Grid = G
    { _data :: !(M.Map Coord Char)
    , width :: !Int
    , height :: !Int
    }
    deriving (Show)

parseGrid :: String -> Grid
parseGrid g = G m w h
  where
    xs = zip [0 :: Int ..] $ lines g
    h = length xs
    w = length . snd . head $ xs
    m = M.fromList $ concatMap parseLine xs
    parseLine :: (Int, String) -> [(Coord, Char)]
    parseLine (x, l) = zipWith (\y c -> ((x, y), c)) [0 ..] l

(@) :: Grid -> Coord -> Maybe Char
(G{..}) @ c = M.lookup c _data

deltas :: [(Int, Int)]
deltas = [(0, 1), (1, 0), (-1, 0), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]

isAdjacentToSymbol :: Grid -> Coord -> Bool
isAdjacentToSymbol g (x, y) = or $ catMaybes isSymbols
  where
    adjacents =
        [ g @ (x + dx, y + dy)
        | (dx, dy) <- deltas
        ]
    isSymbols = map (fmap (\c -> not (isDigit c) && c /= '.')) adjacents

parseDigits :: Grid -> Coord -> Maybe ([Coord], Int)
parseDigits g (x, y)
    | null xs = Nothing
    | otherwise = Just $ foldl' f ([], 0) xs
  where
    xs = catMaybes $ takeWhile isJust [parseDigit g (x, y + dy) | dy <- [0 ..]]
    f (c1, i1) (c2, i2) = (c2 : c1, i1 * 10 + i2)

parseDigit :: Grid -> Coord -> Maybe (Coord, Int)
parseDigit g c = do
    v <- g @ c
    guard (isDigit v)
    return (c, ord v - ord '0')

solve1 :: Grid -> Int
solve1 g = go (0, 0) 0
  where
    go coord acc = case g @ coord of
        Nothing -> acc
        Just _ -> case parseDigits g coord of
            Nothing -> go (next g coord) acc
            Just (cs@(c' : _), n) -> case any (isAdjacentToSymbol g) cs of
                True -> go (next g c') (acc + n)
                False -> go (next g c') acc

next :: Grid -> Coord -> Coord
next (G{..}) (x, y)
    | (y + 1) < width = (x, y + 1)
    | otherwise = (x + 1, 0)

part1 :: String -> String
part1 = show . solve1 . parseGrid

part2 :: String -> String
part2 inp = show $ sumRatios (0, 0) 0
  where
    g = parseGrid inp
    cache = buildCache (0, 0) M.empty
    buildCache coord m = case g @ coord of
        Nothing -> m
        Just _ -> case parseDigits g coord of
            Nothing -> buildCache (next g coord) m
            Just (cs@(c' : _), n) -> buildCache (next g c') $ foldl' (\mp k -> M.insert k n mp) m cs
    sumRatios coord@(x, y) acc = case g @ coord of
        Nothing -> acc
        Just '*' ->
            let coords = [(x + dx, y + dy) | (dx, dy) <- deltas]
                validCoords = filter (\c -> maybe False isDigit (g @ c)) coords
                nextC = next g coord
                filterCoords [] = []
                filterCoords [x] = [x]
                filterCoords ((x1, y1) : (x2, y2) : xs)
                    | x1 == x2 && y1 + 1 == y2 = filterCoords ((x2, y2) : xs)
                    | otherwise = (x1, y1) : filterCoords ((x2, y2) : xs)
             in case filterCoords $ sort validCoords of
                    [c1, c2] -> sumRatios nextC $ acc + (cache M.! c1 * cache M.! c2)
                    _ -> sumRatios nextC acc
        Just _ -> sumRatios (next g coord) acc
