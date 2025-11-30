{-# LANGUAGE LambdaCase #-}

module Day15 where

import Data.Bifunctor
import Data.Foldable
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Utils
import Prelude hiding (Left, Right)

type Position = (Int, Int)

data Move = Up | Down | Left | Right
    deriving (Show, Eq)

data Cell = Robot | Wall | Box | Free | BoxL | BoxR
    deriving (Show, Eq)

data Grid = G {_map :: !(M.Map Position Cell), _l :: !Int, _w :: !Int}

parseCell :: Char -> Cell
parseCell = \case
    '#' -> Wall
    '.' -> Free
    'O' -> Box
    '@' -> Robot
    c -> error $ "No parse for cell: " <> [c]

parseMove :: Char -> Move
parseMove = \case
    '<' -> Left
    '>' -> Right
    '^' -> Up
    'v' -> Down
    c -> error $ "No parse for move: " <> [c]

parseMoves :: String -> [Move]
parseMoves = map parseMove . filter (/= '\n')

parseMap :: String -> Grid
parseMap s =
    let (l, w, g) = parseGrid s
     in G (M.fromList $ map (second parseCell) g) l w

showMap :: Grid -> String
showMap (G m l w) =
    let coords = [[(x, y) | x <- [0 .. w - 1]] | y <- [0 .. l - 1]]
        showCell Wall = '#'
        showCell Free = '.'
        showCell Box = 'O'
        showCell Robot = '@'
        showCell BoxL = '['
        showCell BoxR = ']'
     in unlines $ map (map (showCell . fromJust . (`M.lookup` m))) coords

parseInput :: String -> (Grid, [Move])
parseInput s =
    let [g, m] = splitOn "\n\n" s
     in (parseMap g, parseMoves m)

explode :: Grid -> Grid
explode (G m l w) =
    let
        explodeCell :: (Position, Cell) -> [(Position, Cell)]
        explodeCell ((x, y), c) = case c of
            Robot -> [((2 * x, y), Robot), ((2 * x + 1, y), Free)]
            Box -> [((2 * x, y), BoxL), ((2 * x + 1, y), BoxR)]
            _ -> [((2 * x, y), c), ((2 * x + 1, y), c)]
        m' = M.fromList $ concatMap explodeCell $ M.toList m
     in
        G m' l (w * 2)

move :: Move -> M.Map Position Cell -> Position -> (M.Map Position Cell, Position)
move m g r = case willMove g (next m r) m of
    Nothing -> (g, r)
    Just boxes -> (M.union (M.fromList (((r, Free) : (next m r, Robot) : map (second (const Free)) boxes) <> map (first (next m)) boxes)) g, next m r)

next :: Move -> Position -> Position
next Up = second (subtract 1)
next Down = second (+ 1)
next Left = first (subtract 1)
next Right = first (+ 1)

willMove :: M.Map Position Cell -> Position -> Move -> Maybe [(Position, Cell)]
willMove g p m =
    if m == Left || m == Right
        then case fromJust (M.lookup p g) of
            Free -> Just []
            Box -> ((p, Box) :) <$> willMove g (next m p) m
            BoxL -> ((p, BoxL) :) <$> willMove g (next m p) m
            BoxR -> ((p, BoxR) :) <$> willMove g (next m p) m
            Wall -> Nothing
            Robot -> error "Did not expect robot "
        else case fromJust (M.lookup p g) of
            Free -> Just []
            Box -> ((p, Box) :) <$> willMove g (next m p) m
            BoxL -> do
                sameY <- willMove g (next m p) m
                rightOfThis <- willMove g (next m (next Right p)) m
                return $ ((p, BoxL) : (next Right p, BoxR) : sameY) <> rightOfThis
            BoxR -> do
                sameY <- willMove g (next m p) m
                rightOfThis <- willMove g (next m (next Left p)) m
                return $ ((p, BoxR) : (next Left p, BoxL) : sameY) <> rightOfThis
            Wall -> Nothing
            Robot -> error "Did not expect robot "

solve :: [Move] -> M.Map Position Cell -> (M.Map Position Cell, Int)
solve ms m =
    let
        [(robotPosition, _)] = filter ((== Robot) . snd) $ M.toList m
        (fg, _) = foldl' (\s m' -> uncurry (move m') s) (m, robotPosition) ms
     in
        (fg, sum $ map ((\(x, y) -> y * 100 + x) . fst) $ filter ((\c -> (c == BoxL) || (c == Box)) . snd) $ M.toList fg)

main :: IO ()
main = do
    contents <- input 15
    let (G m l w, ms) = parseInput contents
        showMap' m' = showMap (G m' l w)
        (fg, part1) = solve ms m
    putStrLn $ showMap' fg
    print part1
    let (g, _) = parseInput contents
        (G m2 l2 w2) = explode g
        showMap' m' = showMap (G m' l2 w2)
        (fg, part2) = solve ms m2
    putStrLn $ showMap' fg
    print part2
