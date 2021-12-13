module AoC2021.Day13 where


import           Control.Arrow
import           Control.Monad
import           Data.List.Split
import qualified Data.Set                      as S
import           Miloud



type Fold = (Bool, Int)
type Point = (Int, Int)
type Paper = S.Set Point

data Origami = O
    { folds :: [Fold]
    , paper :: Paper
    }
    deriving Show


parseInput :: String -> Origami
parseInput = parseInput . splitOn [""] . lines
  where
    parseInput [points, folds] = O (map parseFold folds) (parsePaper points)
    parseFold f =
        let [text, number] = splitOn "=" f in (last text == 'x', read number)
    parsePaper = S.fromList . map (join (***) read . mkPair . splitOn ",")
    mkPair [x, y] = (x, y)


foldOrigami :: Origami -> Origami
foldOrigami o@(O []       _) = o
foldOrigami (  O (f : fs) p) = O fs $ S.map (applyFold f) p

applyFold :: Fold -> Point -> Point
applyFold f = (fst f ? first $ second) (mirror (snd f))


mirror :: Int -> Int -> Int
mirror n x | x <= n    = x
           | otherwise = 2 * n - x


day13_1 :: String -> String
day13_1 = show . S.size . paper . foldOrigami . parseInput


foldAll :: Origami -> Origami
foldAll = until (null . folds) foldOrigami


showPaper :: Paper -> String
showPaper p = unlines
    [ [ (x, y) `S.member` p ? '#' $ '.' | x <- [0 .. maxX] ]
    | y <- [0 .. maxY]
    ]
  where
    ps   = S.toList p
    maxX = maximum (map fst ps)
    maxY = maximum (map snd ps)


day13_2 :: String -> String
day13_2 = showPaper . paper . foldAll . parseInput
