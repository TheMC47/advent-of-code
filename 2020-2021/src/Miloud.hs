module Miloud where

import           Data.List
import qualified Data.Map                      as M

headMaybe :: [a] -> Maybe a
headMaybe []      = Nothing
headMaybe (x : _) = Just x

(<!!>) :: [a] -> Int -> Maybe a
xs <!!> i | i < length xs = Just $ xs !! i
          | otherwise     = Nothing

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

unlist4 :: [a] -> (a, a, a, a)
unlist4 [a, b, c, d] = (a, b, c, d)

unlist8 :: [a] -> (a, a, a, a, a, a, a, a)
unlist8 [a, b, c, d, e, f, g, h] = (a, b, c, d, e, f, g, h)

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


(?) :: Bool -> a -> a -> a
(c ? a) b | c         = a
          | otherwise = b
