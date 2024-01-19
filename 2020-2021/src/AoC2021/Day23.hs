{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module AoC2021.Day23 where


import           Control.Arrow
import           Control.Monad
import           Data.Maybe
import           Miloud
type Cost = Int
data Amphipod = A | B | C | D
  deriving (Show, Eq, Ord)

data RoomType = Hallway | Room Amphipod
  deriving (Show, Eq,  Ord)

data Location = L RoomType Int
    deriving (Show, Eq, Ord)

data Burrow = Burrow
    { hallway :: [Maybe Amphipod]
    , roomA   :: [Maybe Amphipod]
    , roomB   :: [Maybe Amphipod]
    , roomC   :: [Maybe Amphipod]
    , roomD   :: [Maybe Amphipod]
    }
    deriving Show

destination :: Amphipod -> Burrow -> [Maybe Amphipod]
destination A = roomA
destination B = roomB
destination C = roomC
destination D = roomD

nextBurrows :: Burrow -> [Burrow]
nextBurrows b0 = []


part1 :: Burrow
part1 = uncurry4 (Burrow (replicate 11 Nothing)) . unlist4 $ map
    (map Just)
    [[B, C], [D, B], [A, D], [C, A]]


locations :: Burrow -> [(Location, Amphipod)]
locations Burrow {..} =
    concatMap (uncurry locInRoom) $ (hallway, Hallway) : zip
        [roomA, roomB, roomC, roomD]
        (map Room [A, B, C, D])
  where
    locInRoom r loc = catMaybes $ zipWith (\i a -> (L loc i, ) <$> a) [0 ..] r

toHallway :: Location -> [Location]
toHallway (L (Room a) i) =
    let hallwayInFront = L Hallway $ case a of
            A -> 2
            B -> 4
            C -> 6
            D -> 8
        stepsInRoom | i == 0    = [L (Room a) 1]
                    | otherwise = []
    in  stepsInRoom <> [hallwayInFront]
toHallway h = [h]

moves :: Location -> Amphipod -> Burrow -> [Location]
moves (L Hallway i) a b = maybeToList home
  where
    [d0, d1] = destination a b
    home     = case d0 of
        Just a' -> if a == a' -- destination has the same kind
            then case d1 of
                Just _  -> Nothing
                Nothing -> Just (L (Room a) 1)
            else Nothing
        Nothing -> case d1 of
            Just _  -> Nothing
            Nothing -> Just (L (Room a) 0)
moves (L (Room a') i) a b
    | a /= a' = legalHallway
    | -- either it's in its place, or it's blocked -> either way nothing to do
      i == 0 = []
    | otherwise = case d0 of
        Just a'' -> if a'' == a then [] else legalHallway
    where [d0, d1] = destination a b



legalHallway :: [Location]
legalHallway = map (L Hallway) [0, 1, 3, 5, 7, 9, 10]

hash :: Burrow -> HashedBurrow
hash = unlist8 . locations

type HashedBurrow -- yes I know shut up
    = ( (Location, Amphipod)
      , (Location, Amphipod)
      , (Location, Amphipod)
      , (Location, Amphipod)
      , (Location, Amphipod)
      , (Location, Amphipod)
      , (Location, Amphipod)
      , (Location, Amphipod)
      )
