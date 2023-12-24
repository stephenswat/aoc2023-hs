module Problems.Day21 (solution) where

import Control.Monad.State.Strict (State, execState, get, put)
import Data.Set (Set, singleton, toList, size, fromList, filter)
import Data.Map (filter, keys, findWithDefault)
import Debug.Trace

import Common.Solution (Day, notImplemented)
import Common.Geometry (Grid2D, Point2D, readGrid2DWith, neighbours4, manhattan)

data Tile
    = Start
    | Garden
    | Rock
    deriving (Show, Eq)

readTile :: Char -> Tile
readTile '.' = Garden
readTile '#' = Rock
readTile 'S' = Start

isAccessible :: Tile -> Bool
isAccessible Garden = True
isAccessible Start = True
isAccessible Rock = False

solve :: Integer -> Grid2D Tile -> Integer
solve m g = toInteger . size . execState (bfs m) $ (singleton s)
    where
        s = head . keys . Data.Map.filter (== Start) $ g
        (valX, valY) = unzip . keys $ g
        maxX = maximum valX
        maxY = maximum valY
        bfs :: Integer -> State (Set Point2D) ()
        bfs n
            | n == 0 = return ()
            | otherwise = do
                q <- get
                let ns = Data.Set.filter (\(x, y) -> isAccessible . findWithDefault Rock (x `mod` maxX, y `mod` maxY) $ g) . fromList . concat . map neighbours4 . toList $ q
                put (trace (let i = size . Data.Set.filter (\(x, y) -> x >= -262 && x <= 327 && y >= -262 && y <= 327) $ ns in ((show (m - n)) ++ ", " ++ (show i))) ns)
                bfs (n - 1)

solution :: Day
solution = (
        show . solve 64 . readGrid2DWith readTile,
        show . solve 1024 . readGrid2DWith readTile
    )
