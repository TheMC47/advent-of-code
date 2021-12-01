{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Maybe
import Day1
import Miloud
import System.Environment
import System.IO
import Text.Read

days :: [String -> String]
days = [day1]

main :: IO ()
main =
  getArgs >>= \case
    (readMaybe -> Just n) : (headMaybe -> fromMaybe "" -> suff)
      | Just solver <- (days <!!> (n - 1)) ->
          openFile ("inputs/day" <> show n <> suff) ReadMode
          >>= hGetContents
          >>= print . solver
    _ -> print "Bye"
