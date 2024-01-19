module AoC2021.Day25 where

import           Control.Monad.State
import           Data.Functor
import           Data.Matrix

type Point = (Int, Int)

data Space = SouthC | EastC | Empty
  deriving Eq

instance Show Space where
    show SouthC = "v"
    show EastC  = ">"
    show Empty  = "."

type Seafloor = Matrix Space

parseInput :: String -> Seafloor
parseInput = fromLists . map (map readS) . lines
  where
    readS c | c == '>'  = EastC
            | c == 'v'  = SouthC
            | otherwise = Empty

moveEast :: Seafloor -> Seafloor
moveEast s = foldr f s (mapPos (,) s)
  where
    (h, w) = liftM2 (,) nrows ncols s
    f ((x, y), sp) s'
        | sp == EastC
        = let dest = (x, if y == w then 1 else y + 1)
          in  if s ! dest == Empty
                  then setElem Empty (x, y) (setElem EastC dest s')
                  else s'
        | otherwise
        = s'

moveSouth :: Seafloor -> Seafloor
moveSouth s = foldr f s (mapPos (,) s)
  where
    (h, w) = liftM2 (,) nrows ncols s
    f ((x, y), sp) s'
        | sp == SouthC
        = let dest = (if x == h then 1 else x + 1, y)
          in  if s ! dest == Empty
                  then setElem Empty (x, y) (setElem SouthC dest s')
                  else s'
        | otherwise
        = s'

migrate :: Seafloor -> [Seafloor]
migrate s =
    let s' = (moveSouth . moveEast) s
    in  if s' == s then [s] else s : migrate s'


day25_1 :: String -> String
day25_1 = show . length . migrate . parseInput
