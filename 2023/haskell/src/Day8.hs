{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day8 where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.List.Split (splitOn)
import qualified Data.Map as M

newtype CellID = CellID {unCellID :: String}
    deriving (Show, Eq, Ord)

data Cell = C {cellID :: !CellID, left :: !CellID, right :: !CellID}
    deriving (Show, Eq)

data Direction = L | R
    deriving (Show, Eq)

newtype Network = N
    {cells :: M.Map CellID Cell}
    deriving (Show, Eq)

mkNetwork :: [Cell] -> Network
mkNetwork = N . M.fromList . map (\c -> (cellID c, c))

data Input = I {directions :: ![Direction], network :: !Network}
    deriving (Show, Eq)

parseDirection :: Char -> Direction
parseDirection 'L' = L
parseDirection 'R' = R
parseDirection _ = error "parseDirection: invalid direction"

parseCell :: String -> Cell
parseCell s =
    let
        [cid, neighbors] = splitOn " = " s
        [l, r] = splitOn ", " (init (tail neighbors))
     in
        C (CellID cid) (CellID l) (CellID r)

parseInput :: String -> Input
parseInput s =
    let
        (dirs : _ : rest) = lines s
     in
        I (map parseDirection dirs) (mkNetwork (map parseCell rest))

data NavigationState = S
    { current :: !CellID
    , step :: !Int
    , visited :: !(M.Map (CellID, Int) Int)
    }
    deriving (Show, Eq)

stateFromCell :: CellID -> NavigationState
stateFromCell c = S c 0 M.empty

newtype MazeNavigation a = MN {runMN :: ReaderT Input (StateT NavigationState Identity) a}
    deriving (Functor, Applicative, Monad, MonadReader Input, MonadState NavigationState)

execMazeNavigation :: MazeNavigation a -> Input -> NavigationState -> (a, NavigationState)
execMazeNavigation prog input st = runIdentity (runStateT (runReaderT (runMN prog) input) st)

wrapStep :: MazeNavigation Int
wrapStep = do
    s <- gets step
    ds <- asks directions
    pure $ s `mod` length ds

getDirection :: MazeNavigation Direction
getDirection = asks ((!!) . directions) <*> wrapStep

getNextCell :: MazeNavigation CellID
getNextCell = do
    n <- asks (cells . network)
    cs <- gets current
    d <- getDirection
    let
        moveTo = case d of
            L -> left
            R -> right
     in
        pure $ (moveTo . (n M.!)) cs

moveToNextCell :: MazeNavigation ()
moveToNextCell = do
    currentCell <- gets current
    nextCell <- getNextCell
    wrappedStep <- wrapStep
    step' <- gets step
    modify'
        ( \s ->
            s
                { current = nextCell
                , step = step' + 1
                , visited = M.insert (currentCell, wrappedStep) step' (visited s)
                }
        )

data CycleSummary = CS
    { start :: !CellID
    , cycleStart :: !Int
    , cycleLength :: !Int
    , targets :: ![CellID]
    , targetCount :: !Int
    , targetSteps :: ![Int]
    , lastSteps :: ![CellID]
    }
    deriving (Show, Eq)

summarizeCycle :: [CellID] -> CycleSummary
summarizeCycle tags =
    let zipped = zip tags [1 :: Int ..]
        targets' = filter (endsWith "Z" . unCellID . fst) $ zipped
        endsWith c = (== c) . drop 2
        cycleStart' = length $ takeWhile ((/= last tags) . fst) zipped
     in CS
            { start = head tags
            , cycleStart = cycleStart'
            , cycleLength = length zipped - cycleStart'
            , targets = map fst targets'
            , targetCount = length . filter (`elem` map fst targets') $ map fst zipped
            , targetSteps = map snd targets'
            , lastSteps = map fst . reverse . take 10 . reverse $ zipped
            }

-- Very unfulfilling solution: each cycle has a single position. Moreover, the
-- cycle length is equal to the position of the target. Which means we just take
-- the LCM of the cycle lengths to get the answer.
run :: IO ()
run = do
    input <- parseInput <$> readFile "input/day8"
    let mazeNavigate :: MazeNavigation [CellID]
        mazeNavigate = do
            currentCell <- gets current
            visitedCells <- gets visited
            currentStep <- wrapStep
            if M.member (currentCell, currentStep) visitedCells
                then pure [currentCell]
                else moveToNextCell >> (currentCell :) <$> mazeNavigate
    let runForS s = fst $ execMazeNavigation mazeNavigate input (stateFromCell (CellID s))
        starts = filter ((== "A") . drop 2) . map unCellID . M.keys . cells . network $ input
    let summarize :: String -> String
        summarize s = show $ summarizeCycle $ runForS s
        results = unlines $ map summarize starts
    putStrLn results
    -- Also write to file
    writeFile "output/day8" results

part1 :: String -> String
part1 = show . solve . parseInput
  where
    mazeNavigation :: MazeNavigation ()
    mazeNavigation = do
        currentCell <- gets current
        if currentCell == CellID "ZZZ"
            then pure ()
            else moveToNextCell >> mazeNavigation
    solve :: Input -> Int
    solve input = step . snd $ execMazeNavigation mazeNavigation input (stateFromCell (CellID "AAA"))

part2 :: String -> String
part2 = show . solve . parseInput
  where
    solve :: Input -> Int
    solve input =
        let starts = filter ((== "A") . drop 2) . map unCellID . M.keys . cells . network $ input
            runForS s = fst $ execMazeNavigation getCycleLength input (stateFromCell (CellID s))
            cycles = map runForS starts
            lcmAll = foldr lcm 1
         in lcmAll cycles
    getCycleLength :: MazeNavigation Int
    getCycleLength = do
        currentCell <- gets current
        if drop 2 (unCellID currentCell) == "Z"
            then gets step
            else moveToNextCell >> getCycleLength
