module Problems.Day23 (solution) where

import Data.Map as M (filterWithKey, findWithDefault, keys)
import Data.Set as S (singleton, size, notMember, insert)
import Data.Function (on)
import Data.Maybe (isJust, fromJust)
import Data.List (maximumBy)

import Common.Solution (Day)
import Common.Geometry (Grid2D, Point2D, readGrid2DWith)
import Common.Cardinal (Direction (..))

data Tile
    = Slope Direction
    | Wall
    | Empty
    deriving (Show, Eq)

readTile :: Char -> Tile
readTile '>' = Slope East
readTile '<' = Slope West
readTile '^' = Slope North
readTile 'v' = Slope South
readTile '#' = Wall
readTile '.' = Empty
readTile _   = error "Invalid tile to read!"

canEnter :: Bool -> Tile -> Direction -> Bool
canEnter _ Wall _ = False
canEnter _ Empty _ = True
canEnter True (Slope _) _ = True
canEnter False (Slope d) d' = d == d'

dir4 :: Point2D -> [(Point2D, Direction)]
dir4 (x, y) = [((x - 1, y), West), ((x + 1, y), East), ((x, y - 1), North), ((x, y + 1), South)]

solve :: Bool -> Grid2D Tile -> Integer
solve p2 g = (+ (-1)) . toInteger . size . fromJust . go s $ singleton s
    where
        (_, valY) = unzip . keys $ g
        s = head . keys . filterWithKey (\(_, y) v -> y == 0 && v == Empty) $ g
        d = head . keys . filterWithKey (\(_, y) v -> y == (maximum valY) && v == Empty) $ g
        go p v
            | p == d = Just v
            | not (null n) = Just (maximumBy (compare `on` size) $ n)
            | otherwise = Nothing
            where
                n = [fromJust r | (np, nd) <- dir4 p, canEnter p2 (findWithDefault Wall np g) nd, notMember np v, let r = go np (insert np v), isJust r]

solution :: Day
solution = (
        show . solve False . readGrid2DWith readTile,
        show . solve True . readGrid2DWith readTile
    )
