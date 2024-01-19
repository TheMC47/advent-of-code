{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module AoC2021.Day21 where



import           Control.Arrow
import           Control.Monad.State
import qualified Data.IntMap                   as IM
import           Data.List
import qualified Data.Map.Strict               as M
import           Data.Maybe
import qualified Data.Vector                   as V

data Player = P1 | P2
  deriving (Show, Eq, Ord)
type Die = [Int]
type Score = Int
type Scores = M.Map Player Score
type Position = Int
type Positions = M.Map Player Position

data Round1 = R
    { players   :: [Player]
    , die       :: Die
    , rolls     :: Int
    , scores    :: Scores
    , positions :: Positions
    }

deterministic :: Die
deterministic = let x = [1 .. 100] <> x in x

players0 :: [Player]
players0 = let x = P1 : P2 : x in x


parseInput :: String -> Positions
parseInput s = M.fromList [(P1, parsePoints p1), (P2, parsePoints p2)]
  where
    [p1, p2] = lines s
    parsePoints p = read $ drop (length "Player 1 starting position: ") p

r0 :: Positions -> Round1
r0 = R players0 deterministic 0 $ M.fromList [(P1, 0), (P2, 0)]

type Game = State Round1

roll :: Game Int
roll = do
    r@R { die, rolls } <- get
    put $ r { die = tail die, rolls = rolls + 1 }
    return $ head die

gameRound :: Game Bool
gameRound =
    replicateM 3 roll >>= moveForward . sum >> (>= 1000) <$> currentScore

play :: Game ()
play = do
    won <- gameRound
    unless won $ do
        modify $ \r -> r { players = tail $ players r }
        play

currentPlayer :: Game Player
currentPlayer = gets (head . players)

currentScore :: Game Score
currentScore = do
    p <- currentPlayer
    gets (fromJust . M.lookup p . scores)


moveForward :: Int -> Game ()
moveForward n = do
    p <- currentPlayer
    modify $ \r -> r { positions = M.adjust (wrap . (+ n)) p (positions r) }
    updateScore

updateScore :: Game ()
updateScore = do
    p   <- currentPlayer
    pos <- gets (fromJust . M.lookup p . positions)
    modify $ \r -> r { scores = M.adjust (+ pos) p (scores r) }


wrap :: Score -> Score
wrap n | n <= 10   = n
       | otherwise = let n' = n `mod` 10 in if n' == 0 then 10 else n'

deterministicGame :: Positions -> Int
deterministicGame ps = rolls * fromJust (M.lookup loser scores)
  where
    R { players, scores, rolls } = execState play $ r0 ps
    loser                        = head . tail $ players

day21_1 :: String -> String
day21_1 = show . deterministicGame . parseInput

play' :: Positions -> (Integer, Integer)
play' ps = dp V.! 0 V.! pos1_0 V.! pos2_0 V.! 0 V.! 0
  where
    pos1_0 = fromJust $ M.lookup P1 ps
    pos2_0 = fromJust $ M.lookup P2 ps
    dp     = V.generate
        2
        (\p -> V.generate
            11
            (\pos1 -> V.generate
                11
                (\pos2 -> V.generate 31 (V.generate 31 . f p pos1 pos2))
            )
        )
    flipP 1 = 0
    flipP 0 = 1
    f player pos1 pos2 score1 score2
        | score1 >= 21
        = (1, 0)
        | score2 >= 21
        = (0, 1)
        | otherwise
        = foldr sumTuple (0, 0)
            $ [ let pos1'   = wrap (if player == 0 then pos1 + r else pos1)
                    pos2'   = wrap (if player == 1 then pos2 + r else pos2)
                    score1' = if player == 0 then score1 + pos1' else score1
                    score2' = if player == 1 then score2 + pos2' else score2
                in  join (***) (* h)
                    $   dp
                    V.! flipP player
                    V.! pos1'
                    V.! pos2'
                    V.! score1'
                    V.! score2'
              | (r, h) <- diracRolls
              ]

diracRolls :: [(Int, Integer)]
diracRolls =
    map (head &&& genericLength)
        $ group
        $ sort
        $ [ x + y + z | x <- [1 .. 3], y <- [1 .. 3], z <- [1 .. 3] ]

type Game' = State (M.Map Round' (Integer, Integer))

type Round' = (Int, Position, Position, Score, Score)

nextPlayer :: Int -> Int
nextPlayer 0 = 1
nextPlayer 1 = 0

play_ :: Round' -> Game' (Integer, Integer)
play_ i@(player, pos1, pos2, score1, score2)
    | score1 >= 21 = return (1, 0)
    | score2 >= 21 = return (0, 1)
    | otherwise = do
        x <- gets (M.lookup i)
        case x of
            Just s  -> return s
            Nothing -> do
                res <- foldr sumTuple (0, 0) <$> sequence
                    [ let
                          pos1' = wrap (if player == 0 then pos1 + r else pos1)
                          pos2' = wrap (if player == 1 then pos2 + r else pos2)
                          score1' =
                              if player == 0 then score1 + pos1' else score1
                          score2' =
                              if player == 1 then score2 + pos2' else score2
                      in
                          do
                              !v <-
                                  join (***) (* h)
                                      <$> play_
                                              ( nextPlayer player
                                              , pos1'
                                              , pos2'
                                              , score1'
                                              , score2'
                                              )
                              return v
                    | (r, h) <- diracRolls
                    ]
                modify (M.insert i res)
                return res

type Game'' = State (IM.IntMap (Integer, Integer))

play_' :: Round' -> Game'' (Integer, Integer)
play_' i@(player, pos1, pos2, score1, score2)
    | score1 >= 1000 = return (1, 0)
    | score2 >= 1000 = return (0, 1)
    | otherwise = do
        let !h = hash i
        x <- gets (IM.lookup h)
        case x of
            Just s  -> return s
            Nothing -> do
                !res <- foldr sumTuple (0, 0) <$> sequence
                    [ let
                          pos1' = wrap (if player == 0 then pos1 + r else pos1)
                          pos2' = wrap (if player == 1 then pos2 + r else pos2)
                          score1' =
                              if player == 0 then score1 + pos1' else score1
                          score2' =
                              if player == 1 then score2 + pos2' else score2
                      in
                          do
                              !v <-
                                  join (***) (* h)
                                      <$> play_'
                                              ( nextPlayer player
                                              , pos1'
                                              , pos2'
                                              , score1'
                                              , score2'
                                              )
                              return v
                    | (r, h) <- diracRolls
                    ]
                modify (IM.insert h res)
                return res

hash :: Round' -> Int
hash (player, pos1, pos2, score1, score2) =
    (((score2 * 10000 + score1) * 100 + pos2) * 100 + pos1) * 10 + player

runPlay :: Positions -> (Integer, Integer)
runPlay ps =
    let pos1_0 = fromJust $ M.lookup P1 ps
        pos2_0 = fromJust $ M.lookup P2 ps
    in  evalState (play_ (0, pos1_0, pos2_0, 0, 0)) M.empty


runPlay' :: Positions -> (Integer, Integer)
runPlay' ps =
    let pos1_0 = fromJust $ M.lookup P1 ps
        pos2_0 = fromJust $ M.lookup P2 ps
    in  evalState (play_' (0, pos1_0, pos2_0, 0, 0)) IM.empty

sumTuple :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
sumTuple (a, b) (c, d) = (a + c, b + d)

day21_2 :: String -> String
day21_2 = show . uncurry max . play' . parseInput


day21_3 :: String -> String
day21_3 = show . uncurry max . runPlay . parseInput


day21_4 :: String -> String
day21_4 = show . uncurry max . runPlay' . parseInput
