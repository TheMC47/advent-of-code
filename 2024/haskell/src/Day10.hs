module Day10 where

import Data.List
import qualified Data.Map as M
import Utils

type Position = (Int, Int)

type Grid = M.Map Position Int

getNeighbors :: Position -> [Position]
getNeighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

parse :: [(Int, Int, Char)] -> Grid
parse = foldr (\(x, y, c) -> M.insert (x, y) (read (pure c))) M.empty

checkPosition :: Grid -> Int -> Position -> Bool
checkPosition g ex xy = Just ex == M.lookup xy g

step :: Grid -> Int -> Position -> [Position]
step g ex p = filter (checkPosition g ex) (getNeighbors p)

startingPoints :: Grid -> [Position]
startingPoints g = filter (checkPosition g 0) $ M.keys g

trail :: Int -> Grid -> [Position] -> [Position]
trail 9 _ ps = ps
trail h g ps = trail (h + 1) g (concatMap (step g (h + 1)) ps)

countTrails :: Grid -> Position -> Int
countTrails g p = length . nub $ trail 0 g [p]

continueTrail :: Int -> Grid -> Position -> [[Position]]
continueTrail 9 _ p = [[p]]
continueTrail h g p =
    let validNextSteps = filter (checkPosition g (h + 1)) (getNeighbors p)
        nextTrails = concatMap (continueTrail (h + 1) g) validNextSteps
     in map (p :) nextTrails

getRating :: Grid -> Position -> Int
getRating g p = length . nub $ continueTrail 0 g p

solve1 :: Grid -> Int
solve1 g = sum $ map (countTrails g) (startingPoints g)

solve2 :: Grid -> Int
solve2 g = sum $ map (getRating g) (startingPoints g)

main :: IO ()
main = do
    (_, _, xs) <- deconstructGrid <$> input 10
    let g = parse xs
    putStrLn $ "Part 1: " <> show (solve1 g)
    putStrLn $ "Part 2: " <> show (solve2 g)
