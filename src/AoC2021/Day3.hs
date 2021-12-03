module AoC2021.Day3 where

import           Data.List
import           Miloud

countBits :: String -> (Int, Int)
countBits = (length <$$>) . partition (== '0')

gammaEpsilon :: [(Int, Int)] -> (Int, Int)
gammaEpsilon [] = (0, 0)
gammaEpsilon ((z, o) : bs) | z > o     = (gamma * 2, 1 + epsilon * 2)
                           | otherwise = (1 + gamma * 2, epsilon * 2)
    where (gamma, epsilon) = gammaEpsilon bs

fromBin :: String -> Int
fromBin = fromBin' . reverse
  where
    fromBin' []         = 0
    fromBin' ('0' : xs) = 2 * fromBin' xs
    fromBin' ('1' : xs) = 1 + 2 * fromBin' xs

day3_1 :: String -> String
day3_1 =
    show
        . uncurry (*)
        . gammaEpsilon
        . reverse
        . map countBits
        . transpose
        . lines

day3_2 :: String -> String
day3_2 =
    show
        . uncurry (*)
        . (\xs -> (solve_3_2 '0' 0 xs, solve_3_2 '1' 0 xs))
        . lines

solve_3_2 :: Char -> Int -> [String] -> Int
solve_3_2 c pos xs = case xs' of
    [x] -> fromBin x
    []  -> 0
    _   -> solve_3_2 c (pos + 1) xs'
  where
    crit = bitCrit c (countBits $ map (!! pos) xs)
    xs'  = filter (crit . (!! pos)) xs

bitCrit :: Char -> (Int, Int) -> Char -> Bool
bitCrit c (z, o) | z > o     = (== c)
                 | otherwise = (/= c)
