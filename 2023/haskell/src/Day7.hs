module Day7 where

import Data.Function
import Data.List

data Part = One | Two
    deriving (Show, Eq, Ord)

data Card = C {part :: !Part, c :: !Char}
    deriving (Show)

cardOrder :: Part -> [Char]
cardOrder One = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']
cardOrder Two = ['A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J']

instance Eq Card where
    (==) = (==) `on` c

instance Ord Card where
    compare = compare `on` cardIndex
      where
        cardIndex :: Card -> Maybe Int
        cardIndex (C p card) = card `elemIndex` cardOrder p

data Hand = H {handPart :: !Part, cards :: ![Card]}
    deriving (Show)

instance Eq Hand where
    (==) = (==) `on` cards

data HandType = FiveOfAKind | FourOfAKind | FullHouse | Flush | Straight | ThreeOfAKind | TwoPair | OnePair | HighCard
    deriving (Show, Eq, Ord)

classifyHand :: Hand -> HandType
classifyHand hand = case handPart hand of
    One -> classify1 $ cards hand
    Two -> classify2 $ cards hand
  where
    countUnique = length . nub
    maxOccur :: [Card] -> Int
    maxOccur = maximum . map length . group . sort

    classify1 :: [Card] -> HandType
    classify1 h
        | countUnique h == 1 = FiveOfAKind
        | countUnique h == 2 && maxOccur h == 4 = FourOfAKind
        | countUnique h == 2 && maxOccur h == 3 = FullHouse
        | maxOccur h == 3 = ThreeOfAKind
        | countUnique h == 3 = TwoPair
        | countUnique h == 4 = OnePair
        | otherwise = HighCard
    classify2 :: [Card] -> HandType
    classify2 h
        | countUnique h == 1 = FiveOfAKind
        | countUnique h == 2 && maxOccur h == 4 = if jokerCount > 0 then FiveOfAKind else FourOfAKind
        | countUnique h == 2 && maxOccur h == 3 = if jokerCount > 0 then FiveOfAKind else FullHouse
        | maxOccur h == 3 = case jokerCount of
            3 -> FourOfAKind
            1 -> FourOfAKind
            _ -> ThreeOfAKind
        | countUnique h == 3 = case jokerCount of
            2 -> FourOfAKind
            1 -> FullHouse
            _ -> TwoPair
        | countUnique h == 4 = case jokerCount of
            2 -> ThreeOfAKind
            1 -> ThreeOfAKind
            _ -> OnePair
        | jokerCount > 0 = OnePair
        | otherwise = HighCard
      where
        jokerCount = length . filter ((== 'J') . c) $ h

-- $> Day7.classifyHand . Day7.parseHand Day7.Two $ "KTJJT"

instance Ord Hand where
    h1 `compare` h2 = case classifyHand h1 `compare` classifyHand h2 of
        EQ -> cards h1 `compare` cards h2
        x -> x

data Bid = B {bidPart :: !Part, bidHand :: !Hand, value :: !Int}
    deriving (Show)

instance Eq Bid where
    (==) (B _ h1 _) (B _ h2 _) = h1 == h2

instance Ord Bid where
    compare = flip compare `on` bidHand

parseHand :: Part -> String -> Hand
parseHand p = H p . map (C p)

parseBid :: Part -> String -> Bid
parseBid p s = B p (parseHand p $ take 5 s) (read $ drop 5 s)

winnings :: [Bid] -> Int
winnings bids = sum $ zipWith (*) (map value $ sort bids) [1 ..]

solve :: Part -> String -> String
solve p = show . winnings . map (parseBid p) . lines

part1 :: String -> String
part1 = solve One

part2 :: String -> String
part2 = solve Two
