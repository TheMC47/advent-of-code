module Day12 where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Utils

type Position = (Int, Int)

type Garden = (M.Map Position Char)

neighbors :: Position -> [Position]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

findAllPlants :: Garden -> S.Set Position -> [Position] -> S.Set Position
findAllPlants _ _ [] = S.empty
findAllPlants g seen (p : ps) =
    if p `S.member` seen
        then findAllPlants g seen ps
        else
            p `S.insert` findAllPlants g (S.insert p seen) (ps <> samePlant g p)

samePlant :: Garden -> Position -> [Position]
samePlant grid p = filter ((M.lookup p grid ==) . (`M.lookup` grid)) (neighbors p)

findPerimeter :: Garden -> [Position] -> Int
findPerimeter grid ps =
    let
        ns = concatMap (map (`M.lookup` grid) . neighbors) ps
        perimeter = filter (M.lookup (head ps) grid /=) ns
     in
        length perimeter

parse :: String -> Garden
parse s = let (_, _, assoc) = parseGrid s in M.fromList assoc

score :: Garden -> [Position] -> Int
score g ps = length ps * findPerimeter g ps

part1 :: Garden -> Int
part1 g = sum $ map ((\p -> length p * findPerimeter g p) . S.toList) (getAllPlants g)

getAllPlants :: Garden -> [S.Set Position]
getAllPlants g = allPlants
  where
    step :: Position -> S.Set Position -> (S.Set Position, S.Set Position)
    step p seen =
        let thesePlants = findAllPlants g seen [p]
         in (thesePlants, S.union seen thesePlants)
    (allPlants, _) = foldr (\k (plants, seen) -> let (newPlants, newSeen) = step k seen in (newPlants : plants, newSeen)) ([], S.empty) (M.keys g)

boundingBox :: [Position] -> (Position, Position)
boundingBox ps =
    let xs = map fst ps
        ys = map snd ps
     in ((minimum xs, minimum ys), (maximum xs, maximum ys))

generateCornerCoordinates :: (Position, Position) -> [Position]
generateCornerCoordinates ((leftx, topy), (rightx, bottomy)) =
    nub
        [ (x, y)
        | x <- [leftx - 1 .. rightx + 1]
        , y <- [topy - 1 .. bottomy + 1]
        ]

countCorners :: S.Set Position -> Position -> Int
countCorners plants (x, y) =
    -- 0 1
    -- 2 3
    case map (`S.member` plants) [(x - 1, y - 1), (x, y - 1), (x - 1, y), (x, y)] of
        -- C C
        -- C C
        [True, True, True, True] -> 0
        -- C C
        -- C
        [True, True, True, False] -> 1
        -- C C
        --   C
        [True, True, False, True] -> 1
        -- C C
        --
        [True, True, False, False] -> 0
        -- C
        -- C C
        [True, False, True, True] -> 1
        -- C
        -- C
        [True, False, True, False] -> 0
        -- C
        --   C
        [True, False, False, True] -> 2
        -- C
        --
        [True, False, False, False] -> 1
        --   C
        -- C C
        [False, True, True, True] -> 1
        --   C
        -- C
        [False, True, True, False] -> 2
        --   C
        --   C
        [False, True, False, True] -> 0
        --   C
        --
        [False, True, False, False] -> 1
        --
        -- C C
        [False, False, True, True] -> 0
        --
        -- C
        [False, False, True, False] -> 1
        --
        --   C
        [False, False, False, True] -> 1
        --
        --
        [False, False, False, False] -> 0
        _ -> 0

countEdges :: S.Set Position -> Int
countEdges ps =
    let
        bb = boundingBox (S.toList ps)
        corners = generateCornerCoordinates bb
     in
        sum $ map (countCorners ps) corners

part2 :: Garden -> Int
part2 g = sum $ map (\p -> S.size p * countEdges p) (filter (not . S.null) (getAllPlants g))

main :: IO ()
main = do
    g <- parse <$> input 12
    print $ part1 g
    print $ part2 g
