module Problems.Day17 (solution) where

import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Map (keys, lookup)

import Common.Cardinal (Direction (..), Rotation (..), translate, rotate)
import Common.Geometry (Grid2D, readGrid2DWith)
import Common.Solution (Day)
import Common.Algorithm (dijkstra)

readDigit :: Char -> Integer
readDigit c = read [c]

solve :: Integer -> Integer -> Grid2D Integer -> Integer
solve minM maxM g = snd . fromJust . dijkstra accept edges $ iNodes
    where
        (valX, valY) = unzip . keys $ g
        iNodes = [(((0, 0), East), 0), (((0, 0), South), 0)]
        accept ((x, y), _) = x == (maximum valX) && y == (maximum valY)
        edges (p, d) =
            [ ((last xps, nr), (sum . catMaybes $ ls))
            | r <- [RotateLeft, RotateRight]
            , s <- [minM..maxM]
            , let nr = rotate r d
            , let xps = take (fromInteger s) . drop 1 . iterate (translate nr) $ p
            , let ls = [Data.Map.lookup i g | i <- xps]
            , and [isJust i | i <- ls]
            ]

solution :: Day
solution = (
        show . solve 1 3 . readGrid2DWith readDigit,
        show . solve 4 10 . readGrid2DWith readDigit
    )
