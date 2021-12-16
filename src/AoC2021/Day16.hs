{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module AoC2021.Day16 where


import           Control.Applicative
import           Control.Arrow
import           Data.Functor
import           Data.Maybe
import           Miloud                         ( uncurry3 )
import           Prelude                 hiding ( EQ
                                                , GT
                                                , LT
                                                )


type Version = Int
type Type = Int

data Mode = M0 | M1
  deriving Show

data Packet = P Version Content
    deriving Show

data OP = Sum | Prod | Min | Max | GT | LT | EQ
  deriving Show

data Content = Literal Int | Operator OP Mode Int [Packet]
  deriving Show

lookupTable :: [(Char, String)]
lookupTable =
    [ ('0', "0000")
    , ('1', "0001")
    , ('2', "0010")
    , ('3', "0011")
    , ('4', "0100")
    , ('5', "0101")
    , ('6', "0110")
    , ('7', "0111")
    , ('8', "1000")
    , ('9', "1001")
    , ('A', "1010")
    , ('B', "1011")
    , ('C', "1100")
    , ('D', "1101")
    , ('E', "1110")
    , ('F', "1111")
    ]


parseInput :: String -> String
parseInput = concat . mapMaybe (`lookup` lookupTable)

newtype Parser a = Parser {runParser :: String -> (Maybe a, String)}

instance Functor Parser  where
    fmap f (Parser p) = Parser $ \s -> let (a, s') = p s in (fmap f a, s')

instance Applicative Parser  where
    pure a = Parser (Just a, )
    (Parser f) <*> (Parser a) = Parser $ \s -> case f s of
        (Just f', s') -> case a s' of
            (Just a', s'') -> (Just $ f' a', s'')
            _              -> (Nothing, s)
        _ -> (Nothing, s)

instance Monad Parser where
    (Parser a) >>= f = Parser $ \s -> case a s of
        (Just a', s') -> runParser (f a') s'
        _             -> (Nothing, s)

char :: Parser Char
char = Parser $ \case
    []       -> (Nothing, [])
    (c : cs) -> (Just c, cs)

manyN :: Int -> Parser a -> Parser [a]
manyN 0 _ = pure []
manyN n p = liftA2 (:) p (manyN (n - 1) p)

manyP :: Parser a -> Parser [a]
manyP p = Parser $ \s -> case runParser p s of
    (Nothing, _ ) -> (Just [], s)
    (Just a , s') -> case runParser (manyP p) s' of
        (Nothing, _  ) -> (Just [a], s')
        (Just as, s'') -> (Just (a : as), s'')

nbits :: Int -> Parser String
nbits n = manyN n char

parseNInt :: Int -> Parser Int
parseNInt n = fromBin <$> nbits n

parseVersion :: Parser Version
parseVersion = parseNInt 3

parseType :: Parser Type
parseType = parseNInt 3

fromBin :: String -> Int
fromBin = fromBin' . reverse
  where
    fromBin' []         = 0
    fromBin' ('0' : xs) = 2 * fromBin' xs
    fromBin' ('1' : xs) = 1 + 2 * fromBin' xs

parseLiteralGroups :: Parser String
parseLiteralGroups = do
    c <- char
    case c of
        '0' -> nbits 4
        '1' -> liftA2 (<>) (nbits 4) parseLiteralGroups

parseLiteral :: Parser Int
parseLiteral = fromBin <$> parseLiteralGroups

restrict :: Int -> Parser a -> Parser a
restrict n (Parser p) = Parser $ \s -> second (<> drop n s) $ p (take n s)

parseMode :: Parser Mode
parseMode = char <&> \case
    '0' -> M0
    '1' -> M1

parseSubPackets :: Parser (Mode, Int, [Packet])
parseSubPackets = do
    m      <- parseMode
    length <- parseLength m
    (m, length, ) <$> case m of
        M1 -> manyN length parsePacket
        M0 -> restrict length (manyP parsePacket)

parseLength :: Mode -> Parser Int
parseLength M0 = parseNInt 15
parseLength M1 = parseNInt 11

typeToOP :: Type -> OP
typeToOP 0 = Sum
typeToOP 1 = Prod
typeToOP 2 = Min
typeToOP 3 = Max
typeToOP 5 = GT
typeToOP 6 = LT
typeToOP 7 = EQ


parseContent :: Parser Content
parseContent = do
    t <- parseType
    if t == 4
        then Literal <$> parseLiteral
        else uncurry3 (Operator (typeToOP t)) <$> parseSubPackets

parsePacket :: Parser Packet
parsePacket = P <$> parseVersion <*> parseContent

sumVersions :: Packet -> Int
sumVersions (P v c) = v + sumVersionsC c

sumVersionsC :: Content -> Int
sumVersionsC (Literal _        ) = 0
sumVersionsC (Operator _ _ _ ps) = sum $ map sumVersions ps

eval :: Packet -> Int
eval (P _ c) = evalC c

evalC :: Content -> Int
evalC (Literal n         ) = n
evalC (Operator op _ _ ps) = case op of
    Sum  -> onPs sum
    Prod -> onPs product
    Min -> onPs minimum
    Max -> onPs maximum
    GT -> cond (>)
    LT -> cond (<)
    EQ -> cond (==)

  where
    onPs f = f $ map eval ps
    cond p = case ps of
      [a1, a2] -> if p (eval a1) (eval a2) then 1 else 0


day16 :: (Packet -> Int) -> String -> String
day16 f = show . f . fromJust . fst . runParser parsePacket . parseInput

day16s = map day16 [sumVersions, eval]
