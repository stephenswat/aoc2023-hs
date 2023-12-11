module Problems.Day11 (solution) where

import Data.Map (keys, mapKeys, filter)

import Common.Solution (Day)
import Common.Geometry (Grid2D, readGrid2DWith, manhattan)

dilate :: Integer -> Grid2D Bool -> Grid2D Bool
dilate v g = mapKeys keyMap g
    where
        (valX, valY) = unzip . keys . Data.Map.filter id $ g
        emptyX = [x | x <- [0..(maximum valX)], not (elem x valX)]
        emptyY = [y | y <- [0..(maximum valY)], not (elem y valY)]
        keyMap (x, y) =
            ( x + (v - 1) * (toInteger . length . Prelude.filter (<= x) $ emptyX)
            , y + (v - 1) * (toInteger . length . Prelude.filter (<= y) $ emptyY)
            )

solve :: Grid2D Bool -> Integer
solve g = sum [manhattan p1 p2 | p1 <- c, p2 <- c, p1 < p2]
    where
        c = keys . Data.Map.filter id $ g

solution :: Day
solution = (
        show . solve . dilate 2       . readGrid2DWith (== '#'),
        show . solve . dilate 1000000 . readGrid2DWith (== '#')
    )
