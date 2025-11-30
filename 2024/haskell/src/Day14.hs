module Day14 where

import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Utils

type Position = (Int, Int)
type Velocity = (Int, Int)
data Robot = R {pos :: !Position, vel :: !Velocity}
    deriving (Show)

tick :: Int -> Robot -> Robot
tick   n (R (x, y) (vx, vy)) = R (x + n * vx, y + n * vy) (vx, vy)

teleport :: Int -> Int -> Robot -> Robot
teleport w l (R (x, y) v) = R (x `mod` w, y `mod` l) v

quadrant :: Int -> Int -> Robot -> Int
quadrant w l (R (x, y) _) =
    let halfw = w `div` 2
        halfl = l `div` 2
     in case (halfw `compare` x, halfl `compare` y) of
            (EQ, _) -> 0
            (_, EQ) -> 0
            (LT, LT) -> 1
            (LT, GT) -> 2
            (GT, LT) -> 3
            (GT, GT) -> 4

parseLine :: String -> Robot
parseLine s =
    let [p, v] = splitOn " " s
        parsePair :: String -> (Int, Int)
        parsePair str = let [a, b] = splitOn "," (drop 2 str) in both read (a, b)
     in R (parsePair p) (parsePair v)

parse :: String -> [Robot]
parse = map parseLine . lines

showMap :: Int -> Int -> [Robot] -> String
showMap w l rs = unlines $ map (map (\xy -> if S.member xy s then '.' else ' ')) coords
  where
    s = S.fromList $ map pos rs
    coords = [[(x, y) | x <- [0 .. w - 1]] | y <- [0 .. l - 1]]

calculateVariance :: [Robot] -> Int
calculateVariance rs =
    let x = length $ nubOrd $ map (fst . pos) rs
        y = length $ nubOrd $ map (snd . pos) rs
     in x * y

main :: IO ()
main = do
    contents <- parse <$> input 14
    let l = 103
        w = 101
        after100 = product . map length . group . sort $ filter (/= 0) $ map (quadrant w l . teleport w l . tick 100) contents
        steps = sortOn (calculateVariance . snd) $ take (w * l) $ zip [0 ..] $ iterate (map (teleport w l . tick 1)) contents
    print $ after100
    forM_ steps $ \(idx, rs) -> do
        putStrLn $ showMap w l rs
        print idx
        putStrLn ""
        putStrLn ""
        void getLine
