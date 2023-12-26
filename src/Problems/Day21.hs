module Problems.Day21 (solution) where

import Data.Set (Set, singleton, toList, size, fromList, filter)
import Data.Map (filter, keys, findWithDefault)

import Common.Solution (Day)
import Common.Geometry (Grid2D, Point2D, readGrid2DWith, neighbours4)

data Tile
    = Start
    | Garden
    | Rock
    deriving (Show, Eq)

readTile :: Char -> Tile
readTile '.' = Garden
readTile '#' = Rock
readTile 'S' = Start
readTile _   = error "Invalid tile to read!"

isAccessible :: Tile -> Bool
isAccessible Garden = True
isAccessible Start = True
isAccessible Rock = False

runBFS :: Integer -> Grid2D Tile -> Set Point2D
runBFS m g = bfs m (singleton s)
    where
        s = head . keys . Data.Map.filter (== Start) $ g
        (valX, valY) = unzip . keys $ g
        maxX = maximum valX
        maxY = maximum valY
        bfs :: Integer -> Set Point2D -> Set Point2D
        bfs n q
            | n == 0 = q
            | otherwise = bfs (n - 1) nq
            where
                ff (x, y)
                    = isAccessible
                    . findWithDefault Rock (x `mod` (maxX + 1), y `mod` (maxY + 1))
                    $ g
                nq
                    = Data.Set.filter ff
                    . fromList
                    . concat
                    . map neighbours4
                    . toList
                    $ q

solveA :: Grid2D Tile -> Integer
solveA = toInteger . size . runBFS 64

solveB :: Grid2D Tile -> Integer
solveB g
    = ((n - 1)^(2 :: Integer) * (countTile 0 0))
    + (n^(2 :: Integer) * (countTile 1 0))
    + (countTile 2 0)
    + (countTile 0 2)
    + (countTile (-2) 0)
    + (countTile 0 (-2))
    + (n - 1) * (countTile 1 1)
    + (n - 1) * (countTile (-1) 1)
    + (n - 1) * (countTile 1 (-1))
    + (n - 1) * (countTile (-1) (-1))
    + n * (countTile 1 2)
    + n * (countTile (-1) 2)
    + n * (countTile 1 (-2))
    + n * (countTile (-1) (-2))
    where
        n = 202300 :: Integer
        (valX, valY) = unzip . keys $ g
        maxX = maximum valX
        maxY = maximum valY
        g1 = runBFS 327 g
        countTile tx ty = toInteger . size . Data.Set.filter ff $ g1
            where
                ff (x, y)
                    = x >= (tx * maxX + tx) && x <= (tx * maxX + maxX + tx) &&
                      y >= (ty * maxY + ty) && y <= (ty * maxY + maxY + ty)

solution :: Day
solution = (
        show . solveA . readGrid2DWith readTile,
        show . solveB . readGrid2DWith readTile
    )
