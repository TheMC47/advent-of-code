{-# LANGUAGE DeriveFunctor #-}
module AoC2021.Day9 where

import           Control.Monad
import           Data.List                      ( nub
                                                , sort
                                                )
import           Data.Vector
import           Miloud
import           Prelude                 hiding ( all
                                                , sum
                                                )

type Grid = Vector (Vector Int)

data Point' a = P
    { _x :: Int
    , _y :: Int
    , _v :: a
    }
    deriving (Functor, Eq, Ord, Show)

type Point = Point' Int

vals :: Vector (Point' a) -> Vector a
vals = Data.Vector.map _v

parseInput :: String -> Grid
parseInput =
    fromList . Prelude.map (fromList . Prelude.map read . chunkify) . lines

neighboursCoord :: Int -> Int -> Vector (Int, Int)
neighboursCoord x y = fromList [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

neighbours :: Grid -> Int -> Int -> Vector Point
neighbours g =
    mapMaybe (\(nx, ny) -> P nx ny <$> ((!? ny) =<< (g !? nx)))
        .: neighboursCoord

lowPoints :: Grid -> Vector Point
lowPoints g = join $ imap (imapMaybe . lowPoint) g
  where
    lowPoint x y v | all (> v) (vals $ neighbours g x y) = Just $ P x y v
                   | otherwise                           = Nothing

day9_1 :: String -> String
day9_1 = show . score . parseInput

score :: Grid -> Int
score = sum . vals . Data.Vector.map (fmap (+ 1)) . lowPoints

day9_2 :: String -> String
day9_2 =
    show
        . Prelude.product
        . Prelude.take 3
        . Prelude.reverse
        . sort
        . basinSizes
        . parseInput

basinSizes :: Grid -> [Int]
basinSizes g = toList $ Data.Vector.map (basinSize . basin g) (lowPoints g)

basinSize :: Vector Point -> Int
basinSize = Prelude.length . nub . toList

crawlUp :: Grid -> Point -> Vector Point
crawlUp g (P x y v) =
    Data.Vector.filter (liftM2 (&&) (> v) (< 9) . _v) $ neighbours g x y

basin :: Grid -> Point -> Vector Point
basin g p = snd bf
  where
    b0 = (singleton p, empty)
    f (bn, acc) = (Data.Vector.concatMap (crawlUp g) bn, acc Data.Vector.++ bn)
    bf = until (Data.Vector.null . fst) f b0
