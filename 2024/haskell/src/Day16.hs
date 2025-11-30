{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day16 where

import Control.Arrow
import Control.Monad.State
import Data.Containers.ListUtils
import Data.Foldable (Foldable (foldl'))
import Data.Functor
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import qualified Data.Heap as H
import Data.List
import Data.Maybe
import Debug.Trace
import GHC.Generics
import Utils

type Position = (Int, Int)
data Direction = S | E | N | W
    deriving (Eq, Generic, Hashable, Show)

type GraphState = (Position, Direction)
data Cell = Free | Wall
    deriving (Eq, Show)

data Graph = Graph
    { maze :: !(HM.HashMap Position Cell)
    , start :: !Position
    , end :: !Position
    , w :: !Int
    , l :: !Int
    }

data Distance = Dist !Int | Infinity
    deriving (Eq, Show)

instance Ord Distance where
    (<=) :: Distance -> Distance -> Bool
    _ <= Infinity = True
    Dist a <= Dist b = a <= b
    Infinity <= Dist _ = False

(+:) :: Distance -> Distance -> Distance
Dist a +: Dist b = Dist (a + b)
_ +: _ = Infinity

(!??) :: (Hashable k, Eq k) => HM.HashMap k Distance -> k -> Distance
m !?? key = fromMaybe Infinity (HM.lookup key m)

data DijkstraState = DijkstraState
    { visitedSet :: !(HS.HashSet GraphState)
    , distanceMap :: !(HM.HashMap GraphState Distance)
    , nodeQueue :: !(H.MinPrioHeap Distance GraphState)
    , parentsMap :: !(HM.HashMap GraphState [GraphState])
    }
    deriving (Show)

emptyDS :: Graph -> DijkstraState
emptyDS (Graph{..}) = DijkstraState HS.empty (HM.singleton (start, E) (Dist 0)) (H.singleton (Dist 0, (start, E))) HM.empty

pos :: GraphState -> Position
pos = fst

move :: GraphState -> GraphState
move ((x, y), s) = case s of
    N -> ((x, y - 1), s)
    W -> ((x - 1, y), s)
    S -> ((x, y + 1), s)
    E -> ((x + 1, y), s)

turnAround :: GraphState -> [GraphState]
turnAround (p, s) =
    map
        (p,)
        (if s == N || s == S then [W, E] else [S, N])

findShortestDistance :: Graph -> Distance
findShortestDistance g@Graph{..} = minimum $ map ((lastState !??) . (end,)) [S, N, W, E] where (lastState, _) = findShortestDistance' g

findShortestDistance' :: Graph -> (HM.HashMap GraphState Distance, HM.HashMap GraphState [GraphState])
findShortestDistance' g@Graph{..} = lastState
  where
    lastState = processQueue (emptyDS g)
    getNeighbors :: GraphState -> [(GraphState, Distance)]
    getNeighbors s = (move s, Dist 1) : map (,Dist 1000) (turnAround s)
    processQueue :: DijkstraState -> (HM.HashMap GraphState Distance, HM.HashMap GraphState [GraphState])
    processQueue ds@(DijkstraState v0 d0 q0 p0) = case H.view q0 of
        Nothing -> (d0, p0)
        Just ((d, st), q1) ->
            if pos st == end
                then (d0, p0)
                else
                    if st `HS.member` v0
                        then processQueue ds{nodeQueue = q1}
                        else
                            let
                                v1 = HS.insert st v0
                                neighbors = getNeighbors st
                                reachable = filter ((Just Free ==) . (`HM.lookup` maze) . fst . fst) neighbors
                                notVisited = filter (not . (`HS.member` v0) . fst) reachable
                             in
                                processQueue $ foldl' (foldNeighbor st) (DijkstraState v1 d0 q1 p0) notVisited
    foldNeighbor :: GraphState -> DijkstraState -> (GraphState, Distance) -> DijkstraState
    foldNeighbor current ds@(DijkstraState _ d0 q1 p0) (neighborNode, cost) =
        let altDistance = (d0 !?? current) +: cost
         in case altDistance `compare` (d0 !?? neighborNode) of
                LT ->
                    ds
                        { nodeQueue = H.insert (altDistance, neighborNode) q1
                        , distanceMap = HM.insert neighborNode altDistance d0
                        , parentsMap = HM.insert neighborNode [current] p0
                        }
                EQ -> ds{parentsMap = HM.adjust (current :) neighborNode p0}
                GT -> ds

data AllPathsState = AllPathsState
    { _visitedSet :: !(HS.HashSet GraphState)
    , _distanceMap :: !(HM.HashMap GraphState (Distance, HS.HashSet GraphState))
    }
    deriving (Show)

findBestPaths g@Graph{..} = execStateT (visit (start, E)) st0
  where
    getNeighbors :: GraphState -> [(GraphState, Distance)]
    getNeighbors s = filter ((Just Free ==) . (`HM.lookup` maze) . fst . fst) $ (move s, Dist 1) : map (,Dist 1000) (turnAround s)

    st0 :: AllPathsState
    st0 = AllPathsState HS.empty (HM.singleton (start, E) (Dist 0, HS.empty))

    visit :: GraphState -> StateT AllPathsState IO ()
    visit _from = do
        modify $ \a -> a{_visitedSet = _from `HS.insert` _visitedSet a}
        (fromCosts, _) <- getBest _from
        let neighbors = map (second (+: fromCosts)) (getNeighbors _from)
        forM_ neighbors $ \(_to, costs) -> do
            (currentCost, currentParents) <- getBest _to
            if costs > currentCost
                then return ()
                else
                    if costs < currentCost
                        then updateMap _to (costs, HS.singleton _from) >> modify (\a -> a{_visitedSet = _to `HS.delete` _visitedSet a})
                        else updateMap _to (costs, HS.insert _from currentParents)
        forM_ (map fst neighbors) $ \_to -> do
            visited <- gets (HS.member _to . _visitedSet)
            if visited then return () else visit _to

    getBest :: GraphState -> StateT AllPathsState IO (Distance, HS.HashSet GraphState)
    getBest _to =
        gets (fromMaybe (Infinity, HS.empty) . HM.lookup _to . _distanceMap)
    updateMap :: GraphState -> (Distance, HS.HashSet GraphState) -> StateT AllPathsState IO ()
    updateMap _to newData = modify $ \a@(AllPathsState{..}) -> a{_distanceMap = HM.insert _to newData _distanceMap}
    debug :: StateT AllPathsState IO ()
    debug = get >>= (liftIO . print) >> void (liftIO getLine)

parse :: String -> Graph
parse s =
    let (l, w, g) = parseGrid s
        parseC '#' = Wall
        parseC _ = Free
        startPosition = fst . head $ filter ((== 'S') . snd) g
        endPosition = fst . head $ filter ((== 'E') . snd) g
        asHM = HM.fromList $ map (second parseC) g
     in Graph
            { maze = asHM
            , start = startPosition
            , end = endPosition
            , w = w
            , l = l
            }

getAllPaths :: GraphState -> HM.HashMap GraphState [GraphState] -> HS.HashSet GraphState
getAllPaths fr parents = case HM.lookup fr parents of
    Nothing -> HS.empty
    Just ps -> fr `HS.insert` HS.unions (map (`getAllPaths` parents) ps)

main :: IO ()
main = do
    contents <- parse <$> input 16
    let shortest = findShortestDistance contents
    print shortest
    let (a, b) = findShortestDistance' contents
    print $ length $ nubOrd $ map fst $ HS.toList $ getAllPaths ((139, 1), E) b
