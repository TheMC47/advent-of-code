{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ

import qualified Day2 as D2

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [day2Tests]

example2 :: String
example2 =
    [r|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|]

day2Tests :: TestTree
day2Tests =
    testGroup
        "Day2"
        $ exampleTests example2 D2.part1 "8" D2.part2 "2286"

exampleTests :: String -> (String -> String) -> String -> (String -> String) -> String -> [TestTree]
exampleTests i p1 s1 p2 s2 =
    [ testCase "part1 Example" $
        p1 i @?= s1
    , testCase "part2 Example" $
        p2 i @?= s2
    ]
