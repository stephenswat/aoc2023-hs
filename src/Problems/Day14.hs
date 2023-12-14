module Problems.Day14 (solution) where

import Data.Map (keys, filter, insert, lookup, empty, filterWithKey)

import Common.Helper (runWhile)
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

solveA :: Grid2D Tile -> Integer
solveA = score . tiltGrid North

solveB :: Grid2D Tile -> Integer
solveB v = score . go (0 :: Integer) (1000000000 :: Integer) empty $ v
    where
        go i d m g
            | i == d = g
            | (Just k) <- l =
                if (d - i) >= (i - k)
                then go (i + ((d - i) `div` (i - k)) * (i - k)) d m g
                else go (i + 1) d m (cycleGrid g)
            | otherwise = go (i + 1) d (insert g i m) (cycleGrid g)
            where
                l = Data.Map.lookup g m

solution :: Day
solution = (
        show . solveA . readGrid2DWith readTile,
        show . solveB . readGrid2DWith readTile
    )
