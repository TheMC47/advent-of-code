{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AoC2021.Day12 where


import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.Functor
import           Data.List.Split
import qualified Data.Map                      as M
import           Data.Set                hiding ( filter
                                                , foldr
                                                , map
                                                )
import           Miloud


type Node = String
type Graph = M.Map Node [Node]
type SeenSet = Set Node


adj :: Node -> Graph -> [Node]
adj = M.findWithDefault []

addAdj :: Node -> Node -> Graph -> Graph
addAdj u v g = M.insert u (v : adj u g) g

addAdj2 :: Node -> Node -> Graph -> Graph
addAdj2 u v g = addAdj v u (addAdj u v g)

parseInput :: String -> Graph
parseInput =
    foldr (uncurry addAdj2 . (\[x, y] -> (x, y)) . splitOn "-") M.empty . lines

newtype DFS a = D (StateT SeenSet  (ReaderT Graph Identity) a)
  deriving (Functor, Monad, MonadState SeenSet, MonadReader Graph)

instance (Semigroup a) => Semigroup (DFS a) where
    (<>) = liftM2 (<>)

instance (Monoid a) => Monoid (DFS a) where
    mempty = return mempty

instance Applicative DFS where
    pure  = return
    (<*>) = ap

runDFS :: DFS a -> SeenSet -> Graph -> (a, SeenSet)
runDFS (D x) = runIdentity .: (runReaderT . runStateT x)

small :: Node -> Bool
small = all isLower

getAdj :: Node -> DFS [Node]
getAdj u = asks (adj u)

see :: Node -> DFS ()
see = liftM2 ($) (when . small) (modify . insert)

unsee :: Node -> DFS ()
unsee = modify . delete

seen :: Node -> DFS Bool
seen u = get <&> member u

visit :: Node -> DFS [[Node]]
visit u = if u == "end"
    then return [["end"]]
    else do
        see u
        adjs <- filterM (fmap not . seen) =<< getAdj u
        xs   <- traverse visit adjs
        unsee u
        return . map (u :) . concat $ xs


allPaths :: Graph -> [[Node]]
allPaths = fst . liftM2 runDFS visit singleton "start"


day12_1 :: String -> String
day12_1 = show . length . allPaths . parseInput
type Seen' = M.Map Node Int

see' :: Node -> Seen' -> Seen'
see' n s | small n   = updateDefault (+ 1) 1 n s
         | otherwise = s

seen' :: Node -> Seen' -> Bool
seen' n s | n == "start" = True
          | countN == 0  = False
          | countN >= 1  = not (M.null twice)
          | otherwise    = False
  where
    twice  = M.filter (== 2) s
    countN = M.findWithDefault 0 n s


visit' :: Node -> Seen' -> Graph -> [[Node]]
visit' n s g | n == "end" = [["end"]]
             | otherwise  = xs
  where
    s'   = see' n s
    adjs = filter (not . (`seen'` s')) $ adj n g
    xs   = map (n :) $ concatMap (\u -> visit' u s' g) adjs

allPaths' :: Graph -> [[Node]]
allPaths' = visit' "start" M.empty

day12_2 :: String -> String
day12_2 = show . length . allPaths' . parseInput
