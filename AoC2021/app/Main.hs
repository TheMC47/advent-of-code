{-# LANGUAGE LambdaCase #-}

module Main where

import Day1
import System.Environment

main :: IO ()
main =
  getArgs >>= \case
    "1" : _ -> day1
    _ -> print "By"
