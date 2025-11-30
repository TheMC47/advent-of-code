{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Day17 where

import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Numeric
import Utils

data Machine = Machine
    { regA :: !Integer
    , regB :: !Integer
    , regC :: !Integer
    , pointer :: !Integer
    , memory :: ![Integer]
    , halted :: !Bool
    , output :: ![Integer]
    }
    deriving (Show)

data Instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv
    deriving (Show)

parseInstruction :: Integer -> Instruction
parseInstruction = \case
    0 -> Adv
    1 -> Bxl
    2 -> Bst
    3 -> Jnz
    4 -> Bxc
    5 -> Out
    6 -> Bdv
    7 -> Cdv

combo :: Machine -> Integer -> Integer
combo (Machine{..}) = \case
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 3
    4 -> regA
    5 -> regB
    6 -> regC
    7 -> -1

advance :: Machine -> Machine
advance m = m{pointer = pointer m + 2}

tick :: Machine -> Machine
tick m@(Machine{..}) = fromMaybe (m{halted = True}) $ do
    instrEncoded <- memory !? pointer
    let instr = parseInstruction instrEncoded
        op = memory !! (fromIntegral $ (pointer + 1))
    pure $ case instr of
        Adv -> advance $ m{regA = regA `div` (2 ^ combo m op)}
        Bxl -> advance $ m{regB = regB `xor` op}
        Bst -> advance $ m{regB = combo m op `mod` 8}
        Jnz -> if regA == 0 then advance m else m{pointer = op}
        Bxc -> advance $ m{regB = regB `xor` regC}
        Out -> advance $ m{output = (combo m op `mod` 8) : output}
        Bdv -> advance $ m{regB = regA `div` (2 ^ combo m op)}
        Cdv -> advance $ m{regC = regA `div` (2 ^ combo m op)}

run :: Machine -> [Machine]
run = go . iterate tick
  where
    go :: [Machine] -> [Machine]
    go [] = []
    go (m : ms) = if halted m then [m] else m : go ms

parse :: String -> Machine
parse s =
    let [a, b, c, "", m] = lines s
        readReg s' = read $ filter isDigit s'
     in Machine
            { regA = readReg a
            , regB = readReg b
            , regC = readReg c
            , pointer = 0
            , output = []
            , halted = False
            , memory = map read $ splitOn "," $ filter (\c -> isDigit c || c == ',') m
            }
part1 :: Machine -> String
part1 m =
    let mf = last $ run m
        o = intercalate "," (map show $ reverse $ output mf)
     in o

main :: IO ()
main = do
    m <- parse <$> input 17
    print $ part1 m
    print $ part2 m

nextAs :: Integer -> Integer -> Machine -> [Integer]
nextAs toFind currentA m = map (\inc -> currentA * 8 + inc) $ filter ((== toFind) . findLast . setA) [0 .. 7]
  where
    setA inc = m{regA = currentA * 8 + inc}
    findLast = last . output . last . run

part2 :: Machine -> Integer
part2 m = minimum $ step [0] lookingFor
  where
    lookingFor = reverse $ memory m
    step ::
        -- \| Current As
        [Integer] ->
        -- \| Left to match
        [Integer] ->
        -- \| Matching
        [Integer]
    step as [] = as
    step as (o : os) = step (concatMap (\a -> nextAs o a m) as) os
