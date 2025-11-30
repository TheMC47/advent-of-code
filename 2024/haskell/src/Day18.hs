module Day18 where

import Control.Monad
import Data.List.Split
import qualified Data.Set as S
import Utils

type Position = (Int, Int)
type Grid = S.Set Position

fill :: Position -> Grid -> Grid
fill = S.insert

neighbors :: Position -> [Position]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isFree :: Position -> Grid -> Bool
isFree = S.notMember

bound :: Int
bound = 70

step :: State -> State
step (g, seen, ps) = (g, ps `S.union` seen, nexts)
  where
    inBoundaries :: Position -> Bool
    inBoundaries (x, y) = x >= 0 && x <= bound && y >= 0 && y <= bound
    nexts = S.fromList $ concatMap (filter (\n -> S.notMember n seen && isFree n g && inBoundaries n) . neighbors) ps

type State = (Grid, S.Set Position, S.Set Position)

showState :: State -> String
showState (g, s, ns) =
    let coords = [[(x, y) | x <- [0 .. 70]] | y <- [0 .. 70]]
        showCoord xy
            | (not $ isFree xy g) = '#'
            | S.member xy s = 'X'
            | S.member xy ns = 'O'
            | otherwise = '.'
        gAndS = S.intersection g s
        sAndNext = S.intersection ns s
        gAndNext = S.intersection g ns
     in unlines $ map (map showCoord) coords <> map show [gAndS, gAndNext, sAndNext]

explore :: Grid -> [State]
explore g = go (S.empty, S.singleton (0, 0))
  where
    finished :: (Grid, S.Set Position) -> Bool
    finished (seen, nexts) = (bound, bound) `elem` nexts

    go :: (S.Set Position, S.Set Position) -> [State]
    go (seen, ps) =
        let (_, seen', ps') = step (g, seen, ps)
         in if null ps' || S.size seen == S.size seen'
                then []
                else
                    if finished (seen', ps')
                        then [(g, seen', ps')]
                        else (g, seen', ps') : go (seen', ps')

halts :: Grid -> Bool
halts g =
    let (_, _, ps) = last $ explore g
     in (bound, bound) `elem` ps

type Input = [Position]

parseInput :: String -> Input
parseInput s =
    let asLines = lines s
        parseLine s = let [a, b] = splitOn "," s in (read a, read b)
     in map parseLine asLines

part1Debug :: Input -> IO ()
part1Debug s =
    let kilo = take 1024 s
     in forM_ (explore (S.fromList kilo)) $ \s -> do
            putStrLn $ showState s
            void getLine

part1 :: Input -> Int
part1 s =
    let kilo = take 1024 s
     in length (explore (S.fromList kilo))

part2 :: Input -> Position
part2 i = let (_, idx) =  search binary check 1024 (length i)     in i !! (idx - 1)
  where
    check :: Int -> Bool
    check idx =
        not $ halts (S.fromList (take idx i))

main :: IO ()
main = do
    bytes <- parseInput <$> input 18
    print $ part1 bytes
    print $ part2 bytes
