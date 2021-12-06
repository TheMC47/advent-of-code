module Miloud where

import Data.Map

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

(<!!>) :: [a] -> Int -> Maybe a
xs <!!> i
  | i < length xs = Just $ xs !! i
  | otherwise = Nothing

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

(<$$>) :: (a -> b) -> (a, a) -> (b, b)
f <$$> (x, x') = (f x, f x')

updateDefault :: Ord k => (a -> a) -> a -> k -> Map k a -> Map k a
updateDefault f b = alter (maybe (Just b) (Just . f))
