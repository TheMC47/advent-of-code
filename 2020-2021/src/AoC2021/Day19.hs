module AoC2021.Day19 where

import           Control.Arrow
import           Control.Monad
import           Data.Either
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Set                      as S

type Point = (Int, Int, Int)

data Scanner = Sc
    { centers :: [(Int, Point)]
    , beacons :: S.Set Point
    }
    deriving Show

type Transform = Point -> Point

parseInput :: String -> [Scanner]
parseInput =
    zipWith (\i bs -> Sc [(i, (0, 0, 0))] bs) [0 ..]
        . map (S.fromList . map readB . tail)
        . splitOn [""]
        . lines
    where readB s = read $ "(" <> s <> ")"


rotations :: [Transform]
rotations =
    [ \(x, y, z) -> (x, y, z)
    , \(x, y, z) -> (x, -z, y)
    , \(x, y, z) -> (x, -y, -z)
    , \(x, y, z) -> (x, z, -y)
    --
    , \(x, y, z) -> (-x, -y, z)
    , \(x, y, z) -> (-x, z, y)
    , \(x, y, z) -> (-x, y, -z)
    , \(x, y, z) -> (-x, -z, -y)
    --
    , \(x, y, z) -> (y, z, x)
    , \(x, y, z) -> (y, -x, z)
    , \(x, y, z) -> (y, -z, -x)
    , \(x, y, z) -> (y, x, -z)
    --
    , \(x, y, z) -> (-y, -z, x)
    , \(x, y, z) -> (-y, x, z)
    , \(x, y, z) -> (-y, z, -x)
    , \(x, y, z) -> (-y, -x, -z)
    --
    , \(x, y, z) -> (z, x, y)
    , \(x, y, z) -> (z, -y, x)
    , \(x, y, z) -> (z, -x, -y)
    , \(x, y, z) -> (z, y, -x)
    --
    , \(x, y, z) -> (-z, -x, y)
    , \(x, y, z) -> (-z, y, x)
    , \(x, y, z) -> (-z, x, -y)
    , \(x, y, z) -> (-z, -y, -x)
    ]

mkTransform :: Point -> Point -> Transform
mkTransform (x1, y1, z1) (x2, y2, z2) (x', y', z') =
    (x' + (x1 - x2), y' + (y1 - y2), z' + (z1 - z2))

match :: Scanner -> Scanner -> Maybe Scanner
match (Sc centers bs) (Sc centers' bs') =
    case join . find isJust $ map matches ts of
        Just (bst, t) ->
            Just $ Sc (centers <> map (second t) centers') (S.union bs bst)
        Nothing -> Nothing
  where
    ts =
        [ (mkTransform b (st b'), st)
        | b  <- S.elems bs
        , b' <- S.elems bs'
        , st <- rotations
        ]
    matches (t, st) =
        let bst    = S.map (t . st) bs'
            common = bs `S.intersection` bst
        in  if S.size common >= 12 then Just (bst, t . st) else Nothing

match' :: Scanner -> Scanner -> Either Scanner Scanner
match' (Sc centers bs) (Sc centers' bs') =
    case join . find isJust $ map matches ts of
        Just (bst, t) -> Right $ Sc (map (second t) centers') bst
        Nothing       -> Left $ Sc centers' bs'
  where
    ts =
        [ (mkTransform b (st b'), st)
        | b  <- S.elems bs
        , b' <- S.elems bs'
        , st <- rotations
        ]
    matches (t, st) =
        let bst    = S.map (t . st) bs'
            common = bs `S.intersection` bst
        in  if S.size common >= 12 then Just (bst, t . st) else Nothing

matchOne' :: Scanner -> [Scanner] -> Maybe [Scanner]
matchOne' ref ss =
    let (unmatched, matched) = partitionEithers $ map (match' ref) ss
    -- in  Just [unify ref matched]
    in  case matched of
            [] -> Nothing
            _  -> Just $ unmatched <> [unify ref matched]

unify :: Scanner -> [Scanner] -> Scanner
unify = foldl' go
  where
    go (Sc centers bs) (Sc centers' bs') =
        Sc (centers <> centers') (S.union bs bs')


matchOne :: Scanner -> [Scanner] -> Maybe [Scanner]
matchOne ref []       = Nothing
matchOne ref (s : ss) = case match ref s of
    Just s' -> Just $ ss <> [s']
    Nothing -> (s :) <$> matchOne ref ss

matchAll :: [Scanner] -> [Scanner]
matchAll [s     ] = [s]
matchAll (s : ss) = case matchOne s ss of
    Nothing -> matchAll $ s : matchAll ss
    Just s' -> matchAll s'

matchAll' :: [Scanner] -> [Scanner]
matchAll' [s     ] = [s]
matchAll' (s : ss) = case matchOne' s ss of
    Nothing -> matchAll' $ s : matchAll' ss
    Just s' -> matchAll' s'

day19_1 :: String -> String
-- day19_1 = show . S.size . beacons . head . matchAll' . parseInput
day19_1 = show . matchAll . parseInput

maxDistances :: [Point] -> Int
maxDistances bs = maximum [ dist b1 b2 | b1 <- bs, b2 <- bs ]

dist :: Point -> Point -> Int
dist (x, y, z) (x', y', z') = abs (x - x') + abs (y - y') + abs (z - z')

day19_2 :: String -> String
day19_2 =
    show . maxDistances . map snd . centers . head . matchAll' . parseInput
