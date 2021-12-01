module GOL where

data State = Dead | Alive
  deriving (Eq, Show)


data Cell = C Int Int

type Grid = Cell -> State


(-->) :: Bool -> Bool -> Bool
True --> False = False
_    --> _     = True

tick :: Grid -> Grid
tick g (C x y) = next ns (g $ C x y)
  where
    ns = length $ filter
        (== Alive)
        [ g (C (x + i) (y + j))
        | i <- [-1, 0, 1]
        , j <- [-1, 0, 1]
        , (i == j) --> (i /= 0)
        ]
    next 2 Alive = Alive
    next 3 Alive = Alive
    next 3 Dead  = Alive
    next _ _     = Dead

pPrint' :: Int -> Int -> Int -> Int -> Grid -> String
pPrint' x y w h g = unlines $ map
    (\y' -> map (pPrintCell . g . flip C y') [x .. x + w - 1])
    [y .. y + h - 1]
  where
    pPrintCell Alive = '#'
    pPrintCell Dead  = '.'


bindGrid :: Int -> Int -> Grid -> Grid
bindGrid w h =
    flip bind Dead $ \(C x y) -> (0 <= x) && (x < w) && (0 <= y) && (y < h)


bind :: (a -> Bool) -> b -> (a -> b) -> a -> b
bind p dflt f x | not (p x) = dflt
                | otherwise = f x


t (C 1 _) = Alive
t _       = Dead
