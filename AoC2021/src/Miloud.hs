module Miloud where

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
