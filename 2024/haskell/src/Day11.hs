{-# LANGUAGE TypeApplications #-}


module Day11 where

import qualified Data.Map as M
import Utils

type Stones = M.Map Integer Integer

incrOccurence :: Stones -> Integer -> Integer -> Stones
incrOccurence m' k by =
    let newOcc = maybe by (+ by) (M.lookup k m')
     in M.insert k newOcc m'

blinkAll :: Stones -> Stones
blinkAll oldStoneMap = foldr (\newStoneMap oldStone -> blinkSimilarStones oldStoneMap oldStone newStoneMap) M.empty ks
  where
    ks = M.keys oldStoneMap
    blinkSimilarStones :: Stones -> Stones -> Integer -> Stones
    blinkSimilarStones occMap newStoneMap oldStone =
        let Just oldOcc = M.lookup oldStone occMap
            newStones = blink oldStone
         in foldr (\newMap newStone -> incrOccurence newStone newMap oldOcc) newStoneMap newStones

blink :: Integer -> [Integer]
blink 0 = [1]
blink n =
    let sN = show n
        l = length sN
        (left, right) = splitAt (l `div` 2) sN
     in if even l
            then map read [left, right]
            else [n * 2024]

solve :: Int -> Stones -> Integer
solve n m = sum (M.elems ((iterate blinkAll m) !! n))

main :: IO ()
main = do
    contents <- map (read @Integer) . words . head . lines <$> input 11
    let asMap = foldr (\k newStoneMap -> incrOccurence newStoneMap k 1) M.empty contents
    print $ solve 25 asMap
    print $ solve 75 asMap
