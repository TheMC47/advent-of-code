module Day9 where

import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Utils

getInput :: [Int] -> V.Vector Integer
getInput ys = V.fromList xs
  where
    xs = go 0 True ys
    go _ _ [] = []
    go segmentId True (i : is) = replicate i segmentId <> go (segmentId + 1) False is
    go segmentId False (i : is) = replicate i (-1) <> go segmentId True is

parse :: String -> [Int]
parse = map (read . (: "")) . head . lines

untilIO :: (a -> IO Bool) -> (a -> IO a) -> a -> IO a
untilIO p f z = do
    resultP <- p z
    if resultP
        then pure z
        else f z >>= untilIO p f

compact :: V.MVector (VM.PrimState IO) Integer -> IO ()
compact v = join (go <$> getNextFree 0 <*> getNextFilled (VM.length v - 1))
  where
    go :: Int -> Int -> IO ()
    go idxFree idxFilled
        | idxFree > idxFilled = pure ()
        | otherwise = do
            x <- v `VM.read` idxFilled
            VM.write v idxFree x
            VM.write v idxFilled (-1)
            join (go <$> getNextFree idxFree <*> getNextFilled idxFilled)

    getNextFree = untilIO (fmap (== -1) . VM.read v) (pure . (+ 1))
    getNextFilled = untilIO (fmap (/= -1) . VM.read v) (pure . (\x -> x - 1))

debug :: V.MVector (VM.PrimState IO) Integer -> IO ()
debug v = do
    VM.forM_ v $ \x -> putStr $ if x == -1 then "." else show x
    putStrLn ""

compact2 :: V.MVector (VM.PrimState IO) Integer -> IO ()
compact2 v = go =<< getNextFilled (VM.length v - 1)
  where
    go :: Int -> IO ()
    go idxFilled =
        if idxFilled < 0
            then pure ()
            else do
                atFilled <- VM.read v idxFilled
                c <- countEqReversed atFilled idxFilled
                bl <- findEmptyBlock c 0
                if bl == -1 || bl >= idxFilled
                    then do
                        fr <- getNextBlock atFilled idxFilled
                        nxt <- getNextFilled fr
                        go nxt
                    else do
                        forM_ [0 .. c - 1] $ \idx -> do
                            VM.write v (idxFilled - idx) (-1)
                            VM.write v (bl + idx) atFilled
                        nxt <- getNextFilled idxFilled
                        void getLine
                        go nxt

    findEmptyBlock :: Int -> Int -> IO Int
    findEmptyBlock n i = do
        if i > VM.length v
            then pure (-1)
            else do
                atI <- checkNEmpty n i
                if atI then pure i else findEmptyBlock n (i + 1)

    countEqReversed :: Integer -> Int -> IO Int
    countEqReversed n i =
        if i < 0
            then pure 0
            else do
                atI <- VM.read v i
                if atI == n
                    then (+ 1) <$> countEqReversed n (i - 1)
                    else pure 0

    checkNEmpty :: Int -> Int -> IO Bool
    checkNEmpty 0 _ = pure True
    checkNEmpty n i =
        if i >= VM.length v
            then pure False
            else do
                atI <- VM.read v i
                if atI == -1
                    then checkNEmpty (n - 1) (i + 1)
                    else pure False
    getNextBlock b =
        untilIO
            ( \idx -> do
                if idx < 0
                    then pure True
                    else (/= b) <$> VM.read v idx
            )
            (pure . (\x -> x - 1))
    getNextFilled =
        untilIO
            ( \idx -> do
                if idx < 0
                    then pure True
                    else (/= -1) <$> VM.read v idx
            )
            (pure . (\x -> x - 1))

checksum :: V.MVector (VM.PrimState IO) Integer -> IO Integer
checksum = fmap (sum . zipWith (*) [0 ..] . map (max 0) . V.toList) . V.freeze

main :: IO ()
main = do
    contents <- getInput . parse <$> input 9
    v <- V.thaw contents
    compact v
    checksum v >>= (putStrLn . ("Part1: " <>) . show)
    v2 <- V.thaw contents
    compact2 v2
    checksum v2 >>= (putStrLn . ("Part2: " <>) . show)
