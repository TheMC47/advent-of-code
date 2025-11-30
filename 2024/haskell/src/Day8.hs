module Day8 where

import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as M
import Utils

type Position = (Int, Int)

data IMap = IM {antennas :: !(M.Map Char [Position]), l :: !Int, w :: !Int}

addAntenna :: Position -> Char -> M.Map Char [Position] -> M.Map Char [Position]
addAntenna p = M.alter f
  where
    f :: (Maybe [Position] -> Maybe [Position])
    f Nothing = Just [p]
    f (Just ps) = Just (p : ps)

antinodes2P1 :: Position -> Position -> [Position]
antinodes2P1 (x1, y1) (x2, y2) = [(2 * x1 - x2, 2 * y1 - y2), (2 * x2 - x1, 2 * y2 - y1)]

antinodesList :: (Position -> Position -> [Position]) -> [Position] -> [Position]
antinodesList _ [] = []
antinodesList f (x : xs) = concatMap (f x) xs <> antinodesList f xs

countAntinodes :: IMap -> Int
countAntinodes (IM a l w) =
    length
        . nubOrd
        . concatMap
            ( filter
                ( \(x, y) ->
                    x >= 0
                        && y >= 0
                        && x < w
                        && y < l
                )
                . antinodesList antinodes2P1
            )
        $ M.elems a

parse :: String -> IMap
parse s =
    let
        (l, w, xyc) = deconstructGrid s
        as = foldl' (\m (x, y, c) -> if c == '.' then m else addAntenna (x, y) c m) M.empty xyc
     in
        IM as l w

main :: IO ()
main = do
    contents <- parse <$> input 8
    putStrLn $ "Part 1:" <> show (countAntinodes contents)
    putStrLn $ "Part 2:" <> show (countAntinodes2 contents)

pot :: Position -> Position -> [Position]
pot (x1, y1) (x2, y2) =
    let g = gcd (x1 - x2) (y1 - y2)
        x0 = (x1 - x2) `div` g
        y0 = (y1 - y2) `div` g
     in map (\k -> (k * x0 + x1, k * y0 + y1)) [-50 .. 50]


countAntinodes2 :: IMap -> Int
countAntinodes2 (IM a l w) =
    length
        . nubOrd
        . concatMap
            ( filter
                ( \(x, y) ->
                    x >= 0
                        && y >= 0
                        && x < w
                        && y < l
                )
                . antinodesList pot
            )
        $ M.elems a
