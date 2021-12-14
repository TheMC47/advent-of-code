module AoC2021.Day14 where


import           Data.Function
import           Data.List
import           Data.List.Split
import qualified Data.Map                      as M
import qualified Data.MultiSet                 as MS


type Element = Char
type Reactors = MS.MultiSet (Element, Element)
type ElementOccurence = MS.MultiSet Element
type Rule = ((Element, Element), Element)

data Polymer = P
    { pairs  :: Reactors
    , occs   :: ElementOccurence
    , rules' :: [Rule]
    }
    deriving Show

parsePolymer' :: String -> Polymer
parsePolymer' = parseInput . splitOn [""] . lines
  where
    f xs = splitOn [""] $ lines xs
    parseInput [[p], rs] = P (insertPairs p) (MS.fromList p) (map parseRule rs)
    parseRule r = let [[a, b, _], [_, c]] = splitOn "->" r in ((a, b), c)
    insertPairs []           = MS.empty
    insertPairs [_         ] = MS.empty
    insertPairs (x : y : ys) = MS.insert (x, y) $ insertPairs (y : ys)

grow :: Polymer -> Polymer
grow (P p occs rs) = P (trimmed `MS.union` new) occs' rs
  where
    (trimmed, new, occs') = go rs
    go []                   = (p, MS.empty, occs)
    go (((e1, e2), b) : rs) = case MS.occur (e1, e2) p of
        0 -> go rs
        n ->
            let (trimmed, new, occs') = go rs
            in  ( MS.deleteAll (e1, e2) trimmed
                , MS.insertMany (e1, b) n (MS.insertMany (b, e2) n new)
                , MS.insertMany b n occs'
                )

score :: ElementOccurence -> Int
score occ = ((-) `on` snd) max min
  where
    occL = MS.toOccurList occ
    max  = maximumBy (compare `on` snd) occL
    min  = minimumBy (compare `on` snd) occL


day14s :: [String -> String]
day14s = map day14 [10, 40]

day14 :: Int -> String -> String
day14 n =
    show . score . occs . flip (foldr (const grow)) [1 .. n] . parsePolymer'
