{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day6 (main) where

import Control.Monad
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Utils

data Direction = U | D | L | R
    deriving (Show, Eq, Ord)

type Position = (Int, Int)

goRight :: Direction -> Direction
goRight U = R
goRight R = D
goRight D = L
goRight L = U

moveForward :: Direction -> Position -> Position
moveForward f (x, y) = case f of
    U -> (x, y - 1)
    D -> (x, y + 1)
    L -> (x - 1, y)
    R -> (x + 1, y)

data Cell = Free | Occupied | Visited
    deriving (Eq, Show)

data State = S {m :: !(M.Map Position Cell), f :: !Direction, p :: !Position, w :: !Int, l :: !Int, visited :: !(S.Set (Direction, Position))}
    deriving (Show)

next :: State -> State
next s@S{..} =
    let
        p' = moveForward f p
     in
        case M.lookup p' m of
            Nothing -> s{p = p'}
            Just Occupied -> s{m = m, f = goRight f, visited = (f, p) `S.insert` visited}
            Just Free -> s{m = M.insert p' Visited m, p = p', visited = (f, p) `S.insert` visited}
            Just Visited -> s{m = M.insert p' Visited m, p = p', visited = (f, p) `S.insert` visited}

finished :: State -> Bool
finished S{..} = isNothing $ M.lookup p m

findLast :: State -> State
findLast = until finished next

countVisited :: State -> Int
countVisited = length . filter (== Visited) . M.elems . m

parse :: String -> State
parse str =
    let ys = zip [0 ..] (lines str)
        xys = [(x, y, c) | (y, s) <- ys, (x, c) <- zip [0 ..] s]
        emptyS = S M.empty U (0, 0) (length (head $ lines str)) (length ys) S.empty
        insC :: State -> (Int, Int, Char) -> State
        insC s@(S{..}) (x, y, c) = case c of
            '.' -> s{m = M.insert (x, y) Free m}
            '^' -> s{m = M.insert (x, y) Visited m, p = (x, y)}
            '#' -> s{m = M.insert (x, y) Occupied m}
            _ -> error "oups"
     in foldl' insC emptyS xys

solve1 :: String -> String
solve1 = show . countVisited . findLast . parse

showState :: State -> String
showState S{..} =
    let coords = map (\y -> [(x, y) | x <- [0 .. w - 1]]) [0 .. l - 1]
        mapCell :: Position -> String
        mapCell xy = case M.lookup xy m of
            Nothing -> error "oups"
            Just c -> mapCellType c
        mapCellType = \case
            Visited -> "X"
            Free -> "."
            Occupied -> "#"
     in unlines $ map (concatMap mapCell) coords

while :: (a -> Bool) -> a -> (a -> IO a) -> IO ()
while cond s f
    | cond s = f s >>= \s' -> while cond s' f
    | otherwise = pure ()

debug1 :: IO ()
debug1 = do
    hSetBuffering stdout NoBuffering
    contents <- input 6
    let firstState = parse contents
    while (not . finished) firstState $ \s -> do
        putStrLn $ showState s
        putStrLn $ "visited: " <> show (visited s)
        putStr "> "
        void getLine
        pure (next s)

    print $ "Part 1:" <> solve1 contents

main :: IO ()
main = do
    contents <- input 6
    print $ "Part 1:" <> solve1 contents
    print $ "Part 2:" <> (show . tryObstacles . parse) contents

looped :: State -> Bool
looped (S{..}) = (f, p) `S.member` visited

tryObstacles :: State -> Int
tryObstacles s@(S{..}) =
    let (S mFinal _ _ _ _ _) = findLast s
        addObstruction :: Position -> State
        addObstruction xy = s{m = M.insert xy Occupied m}
        potential = map (addObstruction . fst) . filter ((/= p) . fst) . filter ((== Visited) . snd) . M.toList $ mFinal
        loops :: State -> Bool
        loops s'
            | finished s' = False -- We're out
            | looped s' = True -- We looped
            | otherwise = loops (next s')
     in length $ filter loops potential
