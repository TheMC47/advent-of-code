{-# LANGUAGE QuasiQuotes #-}

module Tests where

import qualified Data.Map as M
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ

import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5

example2 :: String
example2 =
    [r|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|]

test_day2 :: [TestTree]
test_day2 = exampleTests example2 D2.part1 "8" D2.part2 "2286"

example3 :: String
example3 =
    [r|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
|]

test_day3 :: [TestTree]
test_day3 =
    exampleTests example3 D3.part1 "4361" D3.part2 "467835"
        <> [ testCase "parseDigits some" $ D3.parseDigits g (0, 0) @?= Just ([(0, 1), (0, 0)], 12)
           , testCase "parseDigits nothing" $ D3.parseDigits g (0, 2) @?= Nothing
           , testCase "parseDigits tillTheEnd" $ D3.parseDigits g (1, 1) @?= Just ([(1, 2), (1, 1)], 23)
           ]
  where
    dat =
        M.fromList
            [ ((0, 0), '1')
            , ((0, 1), '2')
            , ((0, 2), '.')
            , ((1, 0), '#')
            , ((1, 1), '2')
            , ((1, 2), '3')
            , ((2, 0), '3')
            ]
    g = D3.G dat 3 2

example4 =
    [r|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|]

test_day4 :: [TestTree]
test_day4 = exampleTests example4 D4.part1 "13" D4.part2 "30"

example5 =
    [r|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4 |]

test_day5 :: [TestTree]
test_day5 =
    exampleTests example5 D5.part1 "35" D5.part2 "46"

exampleTests :: String -> (String -> String) -> String -> (String -> String) -> String -> [TestTree]
exampleTests i p1 s1 p2 s2 =
    [ testCase "part1 Example" $
        p1 i @?= s1
    , testCase "part2 Example" $
        p2 i @?= s2
    ]
