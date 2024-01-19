module AoC2021.Day8 where


import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Set                      as S

type Number = S.Set Char

data Line = L
    { entries :: [Number]
    , output  :: [Number]
    }

type Input = [Line]

parseInput :: String -> [Line]
parseInput =
    map ((\[x, y] -> L x y) . map (map S.fromList . words) . splitOn "|")
        . lines

day8_1 :: String -> String
day8_1 =
    show
        . sum
        . map (length . filter ((`elem` [2, 3, 4, 7]) . length) . output)
        . parseInput

day8_2 :: String -> String
day8_2 = show . sum . map solve . parseInput
  where
    solve l = toNum. map (fromJust . (`elemIndex` decode l)) $ output l
    toNum [a, b, c, d] = a * 1000 + b * 100 + c * 10 + d

decoders :: [Line -> Number]
decoders = [zero, one, two, three, four, five, six, seven, eight, nine]

decode :: Line -> [Number]
decode l = map ($ l) decoders



filterLength :: Int -> [Number] -> [Number]
filterLength n = filter ((== n) . S.size)

uniqueLength :: Int -> Line -> Number
uniqueLength n = head . filterLength n . entries

zero :: Line -> Number
zero l = head . filter (d `S.notMember`) . filterLength 6 . entries $ l
    where d = getD l

one :: Line -> Number
one = uniqueLength 2

two :: Line -> Number
two l = head . filter cond . filterLength 5 . entries $ l
  where
    _four = four l
    cond n = ((== 2) . S.size) $ _four S.\\ n

three :: Line -> Number
three l = head . filter cond . filterLength 5 . entries $ l
  where
    _four = four l
    _b    = getB l
    cond n = liftM2 (&&) (S.member _b) ((== 1) . S.size) $ _four S.\\ n

four :: Line -> Number
four = uniqueLength 4

five :: Line -> Number
five l = head . filter cond . filterLength 5 . entries $ l
  where
    _four = four l
    _b    = getB l
    cond n = liftM2 (&&) (S.notMember _b) ((== 1) . S.size) $ _four S.\\ n

six :: Line -> Number
six l =
    head
        . filter (liftM2 (&&) (d `elem`) (e `elem`))
        . filterLength 6
        . entries
        $ l
  where
    d = getD l
    e = getE l

seven :: Line -> Number
seven = uniqueLength 3

eight :: Line -> Number
eight = uniqueLength 7

nine :: Line -> Number
nine l =
    head
        . filter (liftM2 (&&) (d `elem`) (e `notElem`))
        . filterLength 6
        . entries
        $ l
  where
    d = getD l
    e = getE l

withBD :: Int -> Line -> Char
withBD n l =
    head
        . head
        . head
        . filter ((== n) . length)
        . group
        . sort
        . map S.toList
        . filterLength 1
        $ mask bd l
    where bd = getBD l

getD :: Line -> Char
getD = withBD 1

getB :: Line -> Char
getB = withBD 2

getA :: Line -> Char
getA = head . S.toList . liftM2 (S.\\) seven one

getBD :: Line -> Number
getBD = liftM2 (S.\\) four one

getE :: Line -> Char
getE = head . S.toList . liftM2 (S.\\) two three

mask :: Number -> Line -> [Number]
mask s = map (s S.\\) . entries
