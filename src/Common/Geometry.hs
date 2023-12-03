module Common.Geometry where

import Data.Map (Map, fromList)

type Point2D = (Integer, Integer)
type Point3D = (Integer, Integer, Integer)

type Grid2D a = Map Point2D a
type Grid3D a = Map Point3D a

neighbours4 :: Point2D -> [Point2D]
neighbours4 (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

neighbours5 :: Point2D -> [Point2D]
neighbours5 p = p:(neighbours4 p)

neighbours8 :: Point2D -> [Point2D]
neighbours8 (x, y) =
    [ (x + dx, y + dy)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dx /= 0 || dy /= 0
    ]

neighbours6 :: Point3D -> [Point3D]
neighbours6 (x, y, z) =
    [ (x + dx, y + dy, z + dz)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dz <- [-1, 0, 1]
    , dx /= 0 || dy /= 0 || dz /= 0
    , (abs dx) + (abs dy) + (abs dz) == 1
    ]

neighbours26 :: Point3D -> [Point3D]
neighbours26 (x, y, z) =
    [ (x + dx, y + dy, z + dz)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dz <- [-1, 0, 1]
    , dx /= 0 || dy /= 0 || dz /= 0
    ]

neighbours9 :: Point2D -> [Point2D]
neighbours9 p = p:(neighbours8 p)

readGrid2DWith :: (Char -> a) -> String -> Grid2D a
readGrid2DWith f s = fromList
    [ ((x, y), f c)
    | (y, r) <- zip [0..] (lines s)
    , (x, c) <- zip [0..] r
    ]

readGrid2D :: Read a => String -> Grid2D a
readGrid2D = readGrid2DWith (\c -> read (c:[]))

manhattan :: Point2D -> Point2D -> Integer
manhattan (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))
