module Problems.Day14 (solution) where

import Data.Map (keys, filter, insert, findWithDefault)

import Common.Helper (iterateCyclic)
import Common.Solution (Day)
import Common.Geometry (Grid2D, readGrid2DWith)
import Common.Cardinal (Direction (..), translate, opposite)

data Tile = Moving | Fixed | Empty deriving (Eq, Show, Ord)

readTile :: Char -> Tile
readTile 'O' = Moving
readTile '#' = Fixed
readTile '.' = Empty
readTile _   = error "Invalid tile to read!"

tiltGrid :: Direction -> Grid2D Tile -> Grid2D Tile
tiltGrid d g
    | null toMove = g
    | otherwise = foldl (\g' (_, t) -> insert t Moving g') (foldl (\g' (f, _) -> insert f Empty g') g toMove) toMove
    where
        go p = case findWithDefault Fixed p g of
            Fixed -> p
            Empty -> fp
            Moving -> translate (opposite d) fp
            where
                fp = go (translate d p)
        toMove = [(p, p') | p <- keys . Data.Map.filter (== Moving) $ g, let p' = go p, p /= p']

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
