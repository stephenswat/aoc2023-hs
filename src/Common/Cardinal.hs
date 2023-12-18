module Common.Cardinal (Direction (..), Rotation (..), rotate, getRotation, translate, translateN, opposite) where

import Common.Geometry (Point2D)

data Direction = North | West | South | East deriving (Eq, Show, Ord)

data Rotation = RotateLeft | RotateRight deriving (Eq, Show)

rotate :: Rotation -> Direction -> Direction
rotate RotateLeft North = West
rotate RotateLeft West = South
rotate RotateLeft South = East
rotate RotateLeft East = North
rotate RotateRight North = East
rotate RotateRight West = North
rotate RotateRight South = West
rotate RotateRight East = South

getRotation :: Direction -> Direction -> Rotation
getRotation North West = RotateLeft
getRotation North East = RotateRight
getRotation West North = RotateRight
getRotation West South = RotateLeft
getRotation South West = RotateRight
getRotation South East = RotateLeft
getRotation East North = RotateLeft
getRotation East South = RotateRight
getRotation _    _     = error "Invalid rotation"

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite West  = East
opposite East  = West

translate :: Direction -> Point2D -> Point2D
translate North (x, y) = (x, y - 1)
translate West  (x, y) = (x - 1, y)
translate South (x, y) = (x, y + 1)
translate East  (x, y) = (x + 1, y)

translateN :: Integral a => Direction -> a -> Point2D -> Point2D
translateN North n (x, y) = (x, y - (fromIntegral n))
translateN West  n (x, y) = (x - (fromIntegral n), y)
translateN South n (x, y) = (x, y + (fromIntegral n))
translateN East  n (x, y) = (x + (fromIntegral n), y)
