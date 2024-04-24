{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day6 where

type Race = (Int, Int)

delta :: (Num a) => a -> a -> a
delta t d = t * t - 4 * d

x1 :: (Floating a) => a -> a -> a
x1 t d = -0.5 * (-t + sqrt (delta t d))

x2 :: (Floating a) => a -> a -> a
x2 t d = -0.5 * (-t - sqrt (delta t d))

solve :: (Floating b) => b -> b -> (b, b)
solve t d = (x1 t d, x2 t d)

marginOfError :: Race -> Int
marginOfError (t, d) =
    let (x1', x2') = solve @Double (fromIntegral t) (fromIntegral d)
        isInt a = a == fromIntegral @Int (round a)
        bottom = if isInt x1' then round x1' + 1 else ceiling x1'
        top = if isInt x2' then round x2' - 1 else floor x2'
     in top - bottom + 1

parse1 :: String -> [Race]
parse1 s =
    let ['T' : 'i' : 'm' : 'e' : ':' : tsStr, 'D' : 'i' : 's' : 't' : 'a' : 'n' : 'c' : 'e' : ':' : dsStr] = lines s
        ts = map read $ words tsStr
        ds = map read $ words dsStr
     in zip ts ds

part1 :: String -> String
part1 = show . product . map marginOfError . parse1

parse2 :: String -> Race
parse2 s =
    let ['T' : 'i' : 'm' : 'e' : ':' : tsStr, 'D' : 'i' : 's' : 't' : 'a' : 'n' : 'c' : 'e' : ':' : dsStr] = lines s
        t = read . concat $ words tsStr
        d = read . concat $ words dsStr
     in (t, d)
part2 :: String -> String
part2 = show . marginOfError . parse2
