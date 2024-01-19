{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module AoC2021.Day11 where


import           Control.Monad
import           Control.Monad.State
import           Data.Vector             hiding ( length
                                                , modify
                                                )
import           Miloud
import           Prelude                 hiding ( all
                                                , concat
                                                , concatMap
                                                , filter
                                                , map
                                                , null
                                                )
import qualified Prelude                       as P
                                         hiding ( concatMap
                                                , null
                                                )

import qualified Data.Set                      as S
import           Debug.Trace

type Grid = Vector (Vector Int)

data Point' a = P
    { _x :: Int
    , _y :: Int
    , _v :: a
    }
    deriving (Functor, Eq, Ord, Show)

type Point = Point' Int

vals :: Vector (Point' a) -> Vector a
vals = Data.Vector.map _v

parseInput :: String -> Grid
parseInput = fromList . P.map (fromList . P.map read . chunkify) . lines

neighboursCoord :: Int -> Int -> Vector (Int, Int)
neighboursCoord x y = fromList
    [ (x + 1, y)
    , (x - 1, y)
    , (x    , y + 1)
    , (x    , y - 1)
    , (x + 1, y + 1)
    , (x - 1, y - 1)
    , (x + 1, y - 1)
    , (x - 1, y + 1)
    ]

neighbours :: Grid -> Int -> Int -> Vector Point
neighbours g =
    mapMaybe (\(nx, ny) -> P nx ny <$> ((!? ny) =<< (g !? nx)))
        .: neighboursCoord

updateAt :: Grid -> Point -> Grid
updateAt g P {..} =
    update g $ fromList [(_x, update (g ! _x) $ fromList [(_y, _v)])]

indexedG :: Vector (Vector a) -> Vector (Vector (Point' a))
indexedG = imap (imap . P)

filterG :: (Point -> Bool) -> Grid -> Vector Point
filterG f = concatMap (filter f) . indexedG

data Iteration = S
    { grid :: Grid
    , seen :: S.Set (Int, Int)
    }

flash :: Point -> State Iteration (Vector Point)
flash p = do
    s <- gets seen
    g <- gets grid
    if (_x p, _y p) `S.notMember` s
        then do
            let ns = map (fmap (+ 1)) $ neighbours g (_x p) (_y p)
                g' = foldl' updateAt g ns
                nextFlash =
                    filter (\(P x y v) -> (v > 9) && (x, y) `S.notMember` s) ns
            put . S g' $ S.insert (_x p, _y p) s
            return nextFlash
        else return empty

flashAll :: Vector Point -> State Iteration ()
flashAll ps = do
    g   <- gets grid
    ps' <- concat . toList <$> P.mapM flash ps
    unless (null ps') $ flashAll ps'


tick :: Grid -> Grid
tick g = map (map $ \v -> if v > 9 then 0 else v) . grid $ execState
    (flashAll toFlash)
    s0
  where
    g'      = map (map (+ 1)) g
    toFlash = filterG ((> 9) . _v) g'
    s0      = S g' S.empty


showG :: Grid -> String
showG = unlines . P.map (P.unwords . P.map show) . toList . map toList

day11_1 :: String -> String
day11_1 = show . run . parseInput
  where
    run g = snd $ P.foldr (const step) (g, 0) [1 .. 100]
    step (g0, s0) = (gn, s0 + length (filterG ((== 0) . _v) gn))
        where gn = tick g0


day11_2 :: String -> String
day11_2 = show . run . parseInput
  where
    run g = snd $ until (null . filterG ((> 0) . _v) . fst) step (g, 0)
    step (g0, s0) = (gn, s0 + 1) where gn = tick g0
