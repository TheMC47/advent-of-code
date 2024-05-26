{-# LANGUAGE RecordWildCards #-}

module Day11 (part1, part2) where

import Data.List (genericLength)
import qualified Data.Set as S

type Coord = (Integer, Integer)
data Universe = Universe {galaxies :: ![Coord], emptyRows :: !(S.Set Integer), emptyCols :: !(S.Set Integer)}
  deriving (Show)

parse :: String -> Universe
parse s = Universe gs (S.fromList er) (S.fromList ec)
 where
  ls = lines s
  nCol = genericLength $ head ls
  nRow = genericLength ls
  gs = [(x, y) | (x, l) <- zip [0 ..] ls, (y, c) <- zip [0 ..] l, c == '#']
  er = filter (`notElem` map fst gs) [0 :: Integer .. nRow - 1]
  ec = filter (`notElem` map snd gs) [0 :: Integer .. nCol - 1]

solve :: Integer -> String -> String
solve w = show . solve' . parse
 where
  solve' :: Universe -> Integer
  solve' Universe{..} = sumDistances galaxies
   where
    weight set c = if c `S.member` set then w else 1
    dist1 set c c' = sum [weight set x | x <- [min c c' + 1 .. max c c']]
    dist (x, y) (x', y') = dist1 emptyRows x x' + dist1 emptyCols y y'
    sumDistances :: [Coord] -> Integer
    sumDistances [] = 0
    sumDistances (g : gs) = sum [dist g g' | g' <- gs] + sumDistances gs

part1 :: String -> String
part1 = solve 2

part2 :: String -> String
part2 = solve 1000000
