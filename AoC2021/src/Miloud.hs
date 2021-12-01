module Miloud where

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

(<!!>) :: [a] -> Int -> Maybe a
xs <!!> i
  | i < length xs = Just $ xs !! i
  | otherwise = Nothing
