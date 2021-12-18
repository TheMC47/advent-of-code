module AoC2021.Day18 where

import           Control.Monad
import           Data.Either
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Miloud
import           Text.Parsec
import           Text.Parsec.String

data SNumber = Regular Int | Pair SNumber SNumber
  deriving (Show, Eq)

parseInput :: String -> [SNumber]
parseInput = map (fromRight undefined . parse (snumber <* eof) "") . lines

int :: Parser Int
int = read <$> many1 digit

regular :: Parser SNumber
regular = Regular <$> int

pair :: Parser SNumber
pair = between (char '[') (char ']') $ do
    left <- snumber
    char ','
    Pair left <$> snumber

snumber :: Parser SNumber
snumber = pair <|> regular

type Explosion = Maybe (SNumber, Int, Int)


explode :: SNumber -> Explosion
explode = explode' 0
  where
    explode' 4 (Pair (Regular l) (Regular r)) = Just (Regular 0, l, r)
    explode' d (Pair l           r          ) = case explode' (d + 1) l of
        Just (s', nl, nr) -> Just (Pair s' (addLeft nr r), nl, 0)
        Nothing           -> case explode' (d + 1) r of
            Just (s', nl, nr) -> Just (Pair (addRight nl l) s', 0, nr)
            Nothing           -> Nothing
    explode' _ _ = Nothing


addLeft :: Int -> SNumber -> SNumber
addLeft n' (Regular n) = Regular (n + n')
addLeft n' (Pair l r ) = Pair (addLeft n' l) r

addRight :: Int -> SNumber -> SNumber
addRight n' (Regular n) = Regular (n + n')
addRight n' (Pair l r ) = Pair l (addRight n' r)

split :: SNumber -> Maybe SNumber
split (Regular n)
    | n < 10
    = Nothing
    | otherwise
    = let n' = n `div` 2 in Just $ Pair (Regular n') $ Regular (n - n')
split (Pair l r) = case split l of
    Just l' -> Just $ Pair l' r
    Nothing -> case split r of
        Nothing -> Nothing
        Just r' -> Just $ Pair l r'

reduce :: SNumber -> SNumber
reduce s = case explode s of
    Just (s', _, _) -> reduce s'
    Nothing         -> maybe s reduce (split s)

addSnail :: SNumber -> SNumber -> SNumber
addSnail = reduce .: Pair

sumSnail :: [SNumber] -> SNumber
sumSnail (r : rs) = foldl addSnail r rs

magnitude :: SNumber -> Int
magnitude (Regular n) = n
magnitude (Pair l r ) = 3 * magnitude l + 2 * magnitude r

day18_1 :: String -> String
day18_1 = show . magnitude . sumSnail . parseInput

highest :: [SNumber] -> Int
highest ss = maximum [ magnitude (addSnail l r) | l <- ss, r <- ss, r /= l ]

day18_2 :: String -> String
day18_2 = show . highest . parseInput
