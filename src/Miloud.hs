module Miloud where

import           Data.List
import qualified Data.Map                      as M

headMaybe :: [a] -> Maybe a
headMaybe []      = Nothing
headMaybe (x : _) = Just x

(<!!>) :: [a] -> Int -> Maybe a
xs <!!> i | i < length xs = Just $ xs !! i
          | otherwise     = Nothing

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

(<$$>) :: (a -> b) -> (a, a) -> (b, b)
f <$$> (x, x') = (f x, f x')

updateDefault :: Ord k => (a -> a) -> a -> k -> M.Map k a -> M.Map k a
updateDefault f b = M.alter (maybe (Just b) (Just . f))


pop :: Ord k => k -> M.Map k a -> (Maybe a, M.Map k a)
pop k m = (M.lookup k m, M.delete k m)

chunkify :: [a] -> [[a]]
chunkify = map (: [])


median :: Ord a => [a] -> a
median xs = (!! (length xs `div` 2)) $ sort xs
