{-# LANGUAGE ExistentialQuantification #-}
module Day3 where
import           Data.List

-- import           Data.Array                     ( (!)
--                                                 , listArray
                                                -- )
import           Data.Monoid
-- step :: Int -> Int
-- step 0 = 0
-- step 1 = 1
-- step n = sum [ step i + step (n - i) | i <- [1 .. n - 1] ]

-- step' :: Int -> Integer
-- step' n = s ! n
--  where
--   s = listArray (0, n) (map go [0 .. n])
--   go 0 = 0
--   go 1 = 1
--   go k = sum [ (s ! i) + (s ! (k - i)) | i <- [1 .. k - 1] ]

type Years = [[(Float, Float)]]

-- Utilities
base :: [a] -> a
base = head

output :: [(b1, b2)] -> [b1]
output = map fst

price :: [(a, b)] -> [b]
price = map snd

reduce :: Num a => [(a, a)] -> a
reduce = getSum . mconcat . map (Sum . uncurry (*))

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

-- GDP
nominalGDP :: Years -> Int -> Float
nominalGDP = reduce .: (!!)

realGDP :: Years -> Int -> Float
realGDP ys y = reduce $ zip (output $ ys !! y) (price $ base ys)

deflator :: Years -> Int -> Float
deflator ys y = nominalGDP ys y / realGDP ys y

-- CPI
basketNow :: Years -> Int -> Float
basketNow ys y = reduce $ zip (output $ base ys) (price $ ys !! y)

basketBase :: Years -> Float
basketBase = flip nominalGDP 0

cpi :: Years -> Int -> Float
cpi ys y = basketNow ys y / basketBase ys

-- Inflation
inflation :: (Years -> Int -> Float) -> Years -> Int -> Float
inflation _ _  0 = 1
inflation p ys y = (p ys y - p ys (y - 1)) / p ys (y - 1)

indicators :: [[(Float, Float)]]
indicators = [[(1000, 1), (1000, 1)], [(900, 1.05), (1200, 0.98)]]

-- main

summary :: Years -> String
summary ys = unlines (map unwords padded)
 where
  afs =
    [ (nominalGDP        , "Nominal GDP")
    , (realGDP           , "Real GDP")
    , (cpi               , "CPI")
    , (deflator          , "GDP Deflator")
    , (inflation cpi     , "Inflation CPI")
    , (inflation deflator, "Inflation Deflator")
    ]
  fs = zipWith ($) (map fst afs) (repeat ys)
  is = [0 .. (length ys - 1)]
  summary' i = map ($ i) fs
  ss = map snd afs : map (map show . summary') is
  width i = maximum $ map (length . (!! i)) ss
  padded = transpose $ map (\(ss', i) -> map (padTo (5 + width i)) ss')
                           (zip (transpose ss) [0 ..])
  padTo i s = replicate (i - length s) ' ' <> s

pairUp :: [a] -> [(a, a)]
pairUp []           = []
pairUp (x : y : xs) = (x, y) : pairUp xs

main :: IO ()
main = interact (summary . map (pairUp . tail . (map read) . words) . lines)
