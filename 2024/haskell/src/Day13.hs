module Day13 where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Utils

type Pair = (Integer, Integer)

type Machine = (Pair, Pair, Pair)

parseLine :: String -> Pair
parseLine str =
    let [f, s] = splitOn "," str
     in ( read (filter isDigit f)
        , read (filter isDigit s)
        )

parseMachine :: String -> Machine
parseMachine s =
    let [aStr, bStr, pStr] = lines s
     in ( parseLine aStr
        , parseLine bStr
        , parseLine pStr
        )

-- A * ax + B * bx = px
-- A * ay + B * by = py
solveMachine :: Machine -> Integer
solveMachine ((ax, ay), (bx, by), (px, py)) =
    if det /= 0
        && a >= 0
        && b >= 0
        && a * ax + b * bx == px
        && a * ay + b * by == py
        then 3 * a + b
        else 0
  where
    det = ax * by - bx * ay
    a = (px * by - py * bx) `div` det
    b = (py * ax - px * ay) `div` det

shiftMachine :: Machine -> Machine
shiftMachine (a, b, (px, py)) = (a, b, (px + 10000000000000, py + 10000000000000))

main :: IO ()
main = do
    c <- input 13
    let machines = map parseMachine (splitOn "\n\n" c)
    print $ sum (map solveMachine machines)
    print $ sum (map (solveMachine.shiftMachine) machines)
