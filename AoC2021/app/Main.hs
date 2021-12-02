{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Maybe
import Day1
import Day2
import Miloud
import System.Environment
import System.IO
import Text.Read

days :: [[String -> String]]
days = [[day1], [day2_1, day2_2]]

main :: IO ()
main =
  getArgs >>= \case
    (readMaybe -> Just day) : (readMaybe -> Just part) : (headMaybe -> fromMaybe "" -> suff)
      | Just solver <- ((<!!> (part - 1)) =<< (days <!!> (day - 1))) -> run solver day suff
    (readMaybe -> Just day) : (headMaybe -> fromMaybe "" -> suff)
      | Just solver <- (headMaybe =<< (days <!!> (day - 1))) -> run solver day suff
    _ -> print "Bye"
  where
    run solver day suff =
      openFile ("inputs/day" <> show day <> suff) ReadMode
        >>= hGetContents
        >>= print . solver
