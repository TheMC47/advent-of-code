{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import AoC
import AoC2021
import Data.Maybe
import Miloud
import System.Environment
import System.IO
import Text.Read

main :: IO ()
main = go aoc2021
  where
    go AoC {..} =
      getArgs >>= \case
        (readMaybe -> Just day) : (readMaybe -> Just part) : (headMaybe -> fromMaybe "" -> suff)
          | Just solver <- ((<!!> (part - 1)) =<< (days <!!> (day - 1))) -> run year solver day suff
        (readMaybe -> Just day) : (headMaybe -> fromMaybe "" -> suff)
          | Just solver <- (headMaybe =<< (days <!!> (day - 1))) -> run year solver day suff
        _ -> print "Bye"
    run year solver day suff =
      openFile ("inputs/" <> show year <> "/day" <> show day <> suff) ReadMode
        >>= hGetContents
        >>= print . solver
