
{-# LANGUAGE TupleSections #-}
module AoC2021.Day17 where



import           Control.Arrow
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Maybe


type Point = (Int, Int)
type Velocity = Int

data Projectile = P
    { point    :: Point
    , velocity :: (Velocity, Velocity)
    }
    deriving Show


data Area = A
    { xRange :: (Int, Int)
    , yRange :: (Int, Int)
    }
    deriving Show

parseInput :: String -> Area
parseInput s = (A `on` parseRange) xs ys
  where
    trimmed  = drop (length "target area: ") s
    [xs, ys] = splitOn ", " trimmed
    parseRange rs =
        let [r1, r2] = splitOn ".." $ drop (length "x=") rs
        in  join (***) read (r1, r2)


step :: Projectile -> Projectile
step (P (x, y) (dx, dy)) = P (x', y') (dx', dy')
  where
    x'  = x + dx
    y'  = y + dy
    dy' = dy - 1
    dx' | dx > 0    = dx - 1
        | dx < 0    = dx + 1
        | otherwise = 0


steps :: (Velocity, Velocity) -> Area -> Maybe [Projectile]
steps ds a = go (P (0, 0) ds)
  where
    go p | lost p a   = Nothing
         | inside p a = Just [p]
         | otherwise  = (p :) <$> go (step p)

inside :: Projectile -> Area -> Bool
inside (P (x, y) _) (A (x1, x2) (y1, y2)) =
    x1 <= x && x <= x2 && y1 <= y && y <= y2

lost :: Projectile -> Area -> Bool
lost (P (x, y) (dx, dy)) (A (x1, x2) (y1, y2)) = y < y1 || x > x2


boundsY :: Area -> (Int, Int)
boundsY (A _ (y1, _)) = (y1, -y1 - 1)

boundsX :: Area -> (Int, Int)
boundsX (A (x1, x2) _) =
    ((floor (sqrt (1 + 8 * fromIntegral x1)) - 1) `div` 2, x2)

solve1 :: Area -> Int
solve1 a = let (_, ymax) = boundsY a in ymax * (ymax + 1) `div` 2

solve2 :: Area -> Int
solve2 a@(A (x1, x2) (y1, y2)) =
    length
        . nub
        $ [ (dx, dy)
          | dx <- uncurry enumFromTo $ boundsX a
          , dy <- uncurry enumFromTo $ boundsY a
          , isJust $ steps (dx, dy) a
          ]


day17_1 :: String -> String
day17_1 = show . solve1 . parseInput


day17_2 :: String -> String
day17_2 = show . solve2 . parseInput
