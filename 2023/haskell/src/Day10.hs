{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day10 (
    part1,
    part2,
) where

import Data.Foldable (find)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Prelude hiding (Left, Right)

type Coord = (Int, Int)
type Pipe = Char

newtype Pipes = Pipes {pipes :: M.Map Coord Pipe} deriving (Show)

data Direction = N | E | S | W deriving (Show)

data Position = Position {coord :: !Coord, direction :: !Direction} deriving (Show)

parse :: String -> Pipes
parse = Pipes . M.fromList . concat . zipWith (\y l -> zipWith (\x c -> ((x, y), c)) [0 ..] l) [0 ..] . lines

getPipe :: Pipes -> Coord -> Pipe
getPipe (Pipes p) c = fromJust $ M.lookup c p

goTo :: Direction -> Coord -> Coord
goTo d = \case
    (x, y) -> case d of
        N -> (x, y - 1)
        E -> (x + 1, y)
        S -> (x, y + 1)
        W -> (x - 1, y)

nextDirection :: Pipe -> Direction -> Direction
nextDirection = \case
    'L' -> \case S -> E; W -> N; _ -> error "Invalid move for L"
    'J' -> \case S -> W; E -> N; _ -> error "Invalid move for J"
    'F' -> \case N -> E; W -> S; _ -> error "Invalid move for F"
    '7' -> \case N -> W; E -> S; _ -> error "Invalid move for 7"
    _ -> id

next :: Pipes -> Position -> Position
next ps (Position{..}) =
    let
        coord' = goTo direction coord
        pipe' = ps `getPipe` coord'
     in
         Position coord' $ nextDirection pipe' direction

startPosition :: Pipes -> Position
startPosition (Pipes p) =
    let
        Just (c, 'S') = find ((== 'S') . snd) $ M.toList p
     in
        Position c W

data Orientation = Clockwise | CounterClockwise deriving (Show)

newtype Cycle = Cycle { path :: [Position]} deriving (Show)

getCycle :: Pipes -> Cycle
getCycle p =
    let s = startPosition p
        path = takeWhile ((/= 'S') . getPipe p . coord) . drop 1 . iterate (next p) $ s
        positions = s : path
     in Cycle  positions

countInside :: Pipes -> Int
countInside ps = length . filter odd $ crossings
  where
    Cycle{..} = getCycle ps
    boundary = S.fromList $ map coord path
    potential = filter ((`S.notMember` boundary)) $ M.keys $ pipes ps
    getCrossings (x, y) = length $ filter ((`elem` ['J', 'L', '|']) . getPipe ps) $ filter (`S.member` boundary) [(x - i, y) | i <- [1 .. x]]
    crossings = map getCrossings potential

solve1 :: Pipes -> Int
solve1 = (`div` 2) . length . path . getCycle

part1 :: String -> String
part1 = show . solve1 . parse

part2 :: String -> String
part2 = show . countInside . parse
