{-# LANGUAGE LambdaCase #-}

module AoC2021.Day15 where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity
import qualified Data.Map                      as M
import           Data.Matrix
import           Data.Maybe
import           Data.PSQueue
import qualified Data.Set                      as S
import           Miloud
import           Prelude                 hiding ( lookup )


type Cave = Matrix Int -- 1-indexing!
type Point = (Int, Int)

type PointQueue = PSQ Point Int
type PointDistances = M.Map Point Int
type SeenPoints = S.Set Point


data DijkstraState = DS
    { q    :: PointQueue
    , seen :: SeenPoints
    , dist :: PointDistances
    }

type Dijkstra a = StateT DijkstraState (ReaderT Cave Identity) a

parseInput :: String -> Cave
parseInput = fromLists . map (map read . chunkify) . lines

neighboursCoord :: Point -> [Point]
neighboursCoord (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

modifyQ :: (PointQueue -> PointQueue) -> Dijkstra ()
modifyQ f = do
    DS q seen dist <- get
    put (DS (f q) seen dist)

modifySeen :: (SeenPoints -> SeenPoints) -> Dijkstra ()
modifySeen f = do
    DS q seen dist <- get
    put (DS q (f seen) dist)

modifyDist :: (PointDistances -> PointDistances) -> Dijkstra ()
modifyDist f = do
    DS q seen dist <- get
    put (DS q seen (f dist))

popMin :: Dijkstra (Binding Point Int)
popMin = do
    q <- gets q
    modifyQ deleteMin
    return . fromJust . findMin $ q

insertP :: Point -> Int -> Dijkstra ()
insertP p d = do
    modifyQ (alter (const $ Just d) p)
    modifyDist (M.alter (const $ Just d) p)


updateDistance :: Int -> Point -> Dijkstra ()
updateDistance d p = do
    cave <- ask
    case uncurry safeGet p cave of
        Just extraD ->
            let newD = d + extraD
            in  modifyQ . flip alter p $ \case
                    Nothing -> Just newD
                    Just d' -> Just $ min d' newD
        Nothing -> return ()

isSeen :: Point -> Dijkstra Bool
isSeen p = gets (S.member p . seen)

visit :: Point -> Dijkstra ()
visit = modifySeen . S.insert

dijkstra :: Point -> Dijkstra Int
dijkstra target = do
    pd <- popMin
    let d = prio pd
        p = key pd
    modifyDist (M.insert p d)
    nextNeighbours <- filterM (fmap not . isSeen) $ neighboursCoord p
    mapM_ (updateDistance d) nextNeighbours
    visit $ key pd
    s <- gets seen
    if target `S.member` s
        then gets (fromJust . (M.lookup target . dist))
        else dijkstra target

solve :: Cave -> Int
solve c = runIdentity $ runReaderT (evalStateT (dijkstra target) s0) c
  where
    target = (nrows c, ncols c)
    s0     = DS (singleton (1, 1) 0) S.empty M.empty

day15_1 :: String -> String
day15_1 = show . solve . parseInput

{-
I feel there's a smarter way to go about this, but.. I'm tired
-}

duplicateCave :: Int -> Cave -> Cave
duplicateCave n = mapPos (const $ wrapRisk . (+ n))

wrapRisk :: Int -> Int
wrapRisk n | n <= 9    = n
           | otherwise = n `mod` 9

fullCave :: Cave -> Cave
fullCave c =
    let r =
            c
                <|> duplicateCave 1 c
                <|> duplicateCave 2 c
                <|> duplicateCave 3 c
                <|> duplicateCave 4 c
    in  r
            <-> duplicateCave 1 r
            <-> duplicateCave 2 r
            <-> duplicateCave 3 r
            <-> duplicateCave 4 r

day15_2 :: String -> String
day15_2 = show . solve . fullCave . parseInput
