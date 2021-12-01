{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

-- import           Control.Exception
-- import qualified Day3                          as D
-- import           System.Posix.Types

-- -- main :: IO ()
-- -- main = D.main

-- getParentPid :: ProcessID -> IO (Maybe ProcessID)
-- getParentPid pid = do
--   let fp = "/proc/" ++ show (toInteger pid) ++ "/stat"
--   econtents <- try $ readFile fp :: IO (Either SomeException String)
--   case econtents of
--     Right (lines -> [words -> (_:_:_: (read -> ppid):_)]) -> return (Just ppid)
--     _ -> return Nothing

-- getParentsPids :: ProcessID -> IO [ProcessID]
-- getParentsPids pid = getParentPid pid >>= \case
--   Just ppid -> (ppid :) <$> getParentsPids ppid
--   Nothing   -> return []


-- main :: IO ()
-- main = do
--   pid <- read <$> getLine
--   getParentsPids pid >>= print . show

import           Data.Char


check :: [Int] -> String
check (x : y : z : xs) | (x + y) `mod` 26 == z = check (y : z : xs)
                       | otherwise             = "NO"
check _ = "YES"


data TO = TO {amount :: Int, price :: Int}

-- main = interact $ check . map (flip (-) 65 . ord) . head . lines
-- main = do
--   contents <- getContents
--   print . unlines . map (const "NO") . lines $ contents

-- main =
--   interact
--     $ show
--     . floor
--     . sqrt
--     . fromIntegral
--     . (read :: String -> Int)
--     . head
--     . lines

solve :: [String] -> Int
solve ((x : x' : xs) : ((y : ys) : xss))
  | y == '*'  = solve $ (y : ys) : xss
  | otherwise = solve $ (x' : xs) : ys : map tail xss

main = interact $ show . solve . tail . lines
