{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day12 where

import Data.Function
import Data.List.Split

data Rule = R Int Int
    deriving (Show, Eq)

type Print = [Int]

data Input = I {rules :: ![Rule], prints :: ![Print]}
    deriving (Show)

parse :: String -> Input
parse s =
    let [rules, prints] = splitOn "\n\n" s
        parseRule sr = let [a, b] = splitOn "|" sr in (R `on` read) a b
        parsePrint sp = map (read @Int) (splitOn "," sp)
     in I (map parseRule (lines rules)) (map parsePrint (lines prints))

filterRules :: Print -> [Rule] -> [Rule]
filterRules ps = filter (\(R a b) -> a `elem` ps && b `elem` ps)

checkPrint :: [Rule] -> Print -> Bool
checkPrint rs ps = checkPrint' ps
  where
    rs' = filterRules ps rs
    checkPrint' :: Print -> Bool
    checkPrint' [] = True
    checkPrint' (p : ps') = all (\other -> R other p `notElem` rs') ps' && checkPrint' ps'

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

part1 :: String -> String
part1 s =
    let (I rs ps) = parse s
        valid = filter (checkPrint rs) ps
     in show . sum . map middle $ valid

fixPrint :: [Rule] -> Print -> Print
fixPrint rs print = sortPrint print []
  where
    rs' = filterRules print rs
    insert :: Int -> Print -> Print
    insert p [] = [p]
    insert p (p' : ps') =
        -- If I don't have a rule that tells me p needs to go before p', then I put it after it
        if R p p' `notElem` rs'
            then p' : insert p ps'
            else p : p' : ps'
    sortPrint :: Print -> Print -> Print
    sortPrint [] acc = acc
    sortPrint (p : ps) acc = sortPrint ps (insert p acc)

part2 :: String -> String
part2 s =
    let (I rs ps) = parse s
        valid = filter (not . checkPrint rs) ps
     in show . sum . map (middle . fixPrint rs) $ valid
