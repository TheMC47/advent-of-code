module Utils where

import System.IO (IOMode (ReadMode), hGetContents, openFile)

input :: Int -> IO String
input i = hGetContents =<< openFile ("src/day" ++ show i ++ ".in") ReadMode

deconstructGrid :: String -> (Int, Int, [(Int, Int, Char)])
deconstructGrid s =
    let
        asLines = lines s
        l = length asLines
        w = length (head asLines)
        ys = zip [0 ..] asLines
        xyc = [(x, y, c) | (y, line) <- ys, (x, c) <- zip [0 ..] line]
     in
        (l, w, xyc)

parseGrid :: String -> (Int, Int, [((Int, Int), Char)])
parseGrid s =
    let
        asLines = lines s
        l = length asLines
        w = length (head asLines)
        ys = zip [0 ..] asLines
        xyc = [((x, y), c) | (y, line) <- ys, (x, c) <- zip [0 ..] line]
     in
        (l, w, xyc)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

(!?) :: (Integral i) => [a] -> i -> Maybe a
xs !? i
    | i >= 0 && fromIntegral i < length xs = Just (xs !! fromIntegral i)
    | otherwise = Nothing

-- p must be monotonic: x <= y ==> p x <= p y
search :: (a -> a -> Maybe a) -> (a -> Bool) -> a -> a -> (a, a)
search mid p = go
  where
    go l r = case mid l r of
        Nothing -> (l, r)
        Just m
            | p m -> go l m
            | otherwise -> go m r

binary :: (Integral a) => a -> a -> Maybe a
binary l r
    | r - l > 1 = Just ((l + r) `div` 2)
    | otherwise = Nothing
