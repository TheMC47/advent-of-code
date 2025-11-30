module Day19 where

import Data.List.Split
import Data.Maybe
import Utils (input)

import qualified Data.Map.Strict as M

-- We import a Data.Map, to act as our Dictionary

data Trie
    = Node String (M.Map Char Trie)
    | Empty (M.Map Char Trie)
    deriving (Eq, Show)

empty :: Trie
empty = Empty M.empty

getChildren :: Trie -> M.Map Char Trie
getChildren (Node _ c) = c
getChildren (Empty c) = c

setChildren :: Trie -> M.Map Char Trie -> Trie
setChildren (Node s _) newChildren = Node s newChildren
setChildren (Empty _) newChildren = Empty newChildren

insert :: String -> Trie -> Trie
insert word trie = recurse word trie
  where
    recurse :: String -> Trie -> Trie
    recurse "" t = Node word (getChildren t)
    recurse (c : rest) t =
        let children = getChildren t
         in case M.lookup c children of
                Just matchingChildNode ->
                    setChildren t (M.insert c (recurse rest matchingChildNode) children)
                Nothing ->
                    setChildren t $ M.insert c (recurse rest empty) children

fromList :: [String] -> Trie
fromList = foldr insert empty

match :: Trie -> String -> Bool
match _ "" = True
match towels toMatch = any (match towels) rests
  where
    try :: String -> Maybe String
    try t =
        let cropped = take (length t) toMatch
         in if cropped == t
                then Just $ drop (length t) toMatch
                else Nothing
    rests = mapMaybe try towels

parse :: String -> ([String], [String])
parse s =
    let (firstLine : "" : patterns) = lines s
     in (splitOn ", " firstLine, patterns)

main :: IO ()
main = do
    (towels, patterns) <- parse <$> input 19
    let xs = filter (match (fromList towels)) patterns
    print $ xs
    print $ length xs
