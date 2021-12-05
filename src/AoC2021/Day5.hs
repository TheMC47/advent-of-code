{-# LANGUAGE RecordWildCards #-}

module AoC2021.Day5 where

import           Data.Function
import           Data.List
import           Data.List.Split

import qualified Data.Map                      as M

type Point = (Int, Int)

data Line = L
    { start  :: Point
    , finish :: Point
    }
    deriving Show


type Grid = M.Map Point Int

data Input = I
    { grid :: Grid
    , ls   :: [Line]
    }

hOrV :: Line -> Bool
hOrV L {..} = ((||) `on` f) fst snd where f g = ((==) `on` g) start finish

line :: [[Int]] -> Line
line [[x1, y1], [x2, y2]] = L (x1, y1) (x2, y2)

unline :: (Point -> Int) -> Line -> [Int]
unline f L {..} = sort [f start, f finish]

day5 :: (Line -> Bool) -> String -> String
day5 f =
    show
        . points
        . markInput
        . I M.empty
        . filter f
        . map (line . map (map read . splitOn ",") . splitOn "->")
        . lines

markInput :: Input -> Grid
markInput I {..} = foldl mark grid ls

mark :: Grid -> Line -> Grid
mark = (. spanLine) . foldl (flip (M.alter (maybe (Just 1) (Just . (+ 1)))))

spanLine :: Line -> [Point]
spanLine l@(L (x1, y1) (x2, y2)) = map
    (\step -> (x1 + xsign * step, y1 + ysign * step))
    [0 .. diff]
  where
    [xmin, xmax] = unline fst l
    [ymin, ymax] = unline snd l
    xsign        = signum (x2 - x1)
    ysign        = signum (y2 - y1)
    diff         = max (xmax - xmin) (ymax - ymin)

points :: Grid -> Int
points = length . filter (>= 2) . map snd . M.toList


day5s = map day5 [hOrV, const True]
