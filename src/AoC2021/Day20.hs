{-# LANGUAGE RecordWildCards #-}

module AoC2021.Day20 where


import           Data.List
import qualified Data.Map                      as M
import qualified Data.Vector                   as V


type Point = (Int, Int)
data Pixel = Dark | Lit
  deriving (Show, Eq)
type MappedPoints = M.Map Point Pixel
type Algorithm = V.Vector Pixel

data Image = I
    { infinity     :: Pixel
    , algorithm    :: Algorithm
    , mappedPoints :: MappedPoints
    }

parseInput :: String -> Image
parseInput s = I Dark a mappedPoints
  where
    (as : _ : im) = lines s
    a             = V.fromList . map toPixel $ as
    mappedPoints  = M.fromList . concat $ zipWith
        (\x l -> zipWith (\y c -> ((x, y), toPixel c)) [0 ..] l)
        [0 ..]
        im

toPixel :: Char -> Pixel
toPixel '.' = Dark
toPixel '#' = Lit

neighbouring :: Point -> [Point]
neighbouring (x, y) = [ (x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1] ]

showI :: Image -> String
showI i = unlines [ [ fromPixelS $ findPixel (x, y) i | y <- ys ] | x <- xs ]
  where
    xs = [minX i - 1 .. maxX i + 1]
    ys = [minY i - 1 .. maxY i + 1]

maxD :: (Point -> Int) -> Image -> Int
maxD d = maximum . map d . M.keys . mappedPoints

minD :: (Point -> Int) -> Image -> Int
minD d = minimum . map d . M.keys . mappedPoints

maxY, maxX, minY, minX :: Image -> Int
maxX = maxD fst
maxY = maxD snd
minX = minD fst
minY = minD snd

enhancePixel :: Image -> Point -> Pixel
enhancePixel i@I {..} p =
    algorithm V.! fromPixels (map (`findPixel` i) $ neighbouring p)


enhance :: Image -> Image
enhance i@I {..} = I (nextInfinity i) algorithm
    $ M.fromList [ ((x, y), enhancePixel i (x, y)) | x <- xs, y <- ys ]
  where
    xs = [minX i - 1 .. maxX i + 1]
    ys = [minY i - 1 .. maxY i + 1]

nextInfinity :: Image -> Pixel
nextInfinity I {..} | algorithm V.! 0 == Dark = Dark
                    | otherwise               = flipP infinity
                    -- Doesn't work if the last one is also lit, but it won't happen

flipP :: Pixel -> Pixel
flipP Lit  = Dark
flipP Dark = Lit

findPixel :: Point -> Image -> Pixel
findPixel p I {..} = M.findWithDefault infinity p mappedPoints

fromPixels :: [Pixel] -> Int
fromPixels = foldl' (\z x -> 2 * z + fromPixel x) 0

fromPixel :: Pixel -> Int
fromPixel Dark = 0
fromPixel Lit  = 1

fromPixelS :: Pixel -> Char
fromPixelS Dark = '.'
fromPixelS Lit  = '#'


day20s :: [String -> String]
day20s = map day20 [2, 50]

day20 :: Int -> String -> String
day20 i =
    show
        . length
        . filter (== Lit)
        . M.elems
        . mappedPoints
        . flip (foldr (const enhance)) [1 .. i]
        . parseInput
