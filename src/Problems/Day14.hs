module Problems.Day14 (solution) where

import Data.Map (keys, filter, insert, lookup, filterWithKey)

import Common.Helper (runWhile, iterateCyclic)
import Common.Solution (Day)
import Common.Geometry (Grid2D, readGrid2DWith)
import Common.Cardinal (Direction (..), translate)

data Tile = Moving | Fixed | Empty deriving (Eq, Show, Ord)

readTile :: Char -> Tile
readTile 'O' = Moving
readTile '#' = Fixed
readTile '.' = Empty
readTile _   = error "Invalid tile to read!"

tiltGrid :: Direction -> Grid2D Tile -> Grid2D Tile
tiltGrid d = runWhile go
    where
        moveTile g p = insert p Empty . insert (translate d p) Moving $ g
        go g
            | null toMove = (g, False)
            | otherwise = (foldl moveTile g toMove, True)
            where
                kvFilter k v = v == Moving && Data.Map.lookup (translate d k) g == (Just Empty)
                toMove = keys . filterWithKey kvFilter $ g

cycleGrid :: Grid2D Tile -> Grid2D Tile
cycleGrid = tiltGrid East . tiltGrid South . tiltGrid West . tiltGrid North

score :: Grid2D Tile -> Integer
score g = sum . map (\y -> (maxY + 1) - y) . map snd . keys . Data.Map.filter (== Moving) $ g
    where
        (_, valY) = unzip . keys $ g
        maxY = maximum valY

solution :: Day
solution = (
        show . score . tiltGrid North . readGrid2DWith readTile,
        show . score . iterateCyclic cycleGrid 1000000000 . readGrid2DWith readTile
    )
