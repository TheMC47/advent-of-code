{-# LANGUAGE QuasiQuotes #-}

module Tests where

import qualified Data.Map as M
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ

import qualified Day2 as D2
import qualified Day3 as D3

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

exampleTests :: String -> (String -> String) -> String -> (String -> String) -> String -> [TestTree]
exampleTests i p1 s1 p2 s2 =
    [ testCase "part1 Example" $
        p1 i @?= s1
    , testCase "part2 Example" $
        p2 i @?= s2
    ]
