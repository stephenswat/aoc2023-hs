module Problems.Day10 (solution) where

import Control.Monad.State.Lazy (execState, get, put)
import Data.Map (Map, toList, insertWith, singleton, findWithDefault, notMember, member, keys)

import Common.Solution (Day)
import Common.Geometry (Grid2D, Point2D, readGrid2DWith)
import Common.Cardinal (Direction (..), opposite)

data Tile
    = PipeNS
    | PipeWE
    | PipeNE
    | PipeNW
    | PipeSW
    | PipeSE
    | Empty
    | Start
    deriving Eq

readTile :: Char -> Tile
readTile '|' = PipeNS
readTile '-' = PipeWE
readTile 'L' = PipeNE
readTile 'J' = PipeNW
readTile '7' = PipeSW
readTile 'F' = PipeSE
readTile '.' = Empty
readTile 'S' = Start
readTile _   = error "Invalid character!"

connects1 :: Tile -> Direction -> Bool
connects1 PipeNS North = True
connects1 PipeNS South = True
connects1 PipeWE West  = True
connects1 PipeWE East  = True
connects1 PipeNE North = True
connects1 PipeNE East  = True
connects1 PipeNW North = True
connects1 PipeNW West  = True
connects1 PipeSW South = True
connects1 PipeSW West  = True
connects1 PipeSE South = True
connects1 PipeSE East  = True
connects1 Start  _     = True
connects1 _      _     = False

connections :: Grid2D Tile -> Point2D -> [Point2D]
connections m o@(x, y) =
    [ p
    | (p, d) <-
        [ ((x + 1, y), East)
        , ((x - 1, y), West)
        , ((x, y - 1), North)
        , ((x, y + 1), South)
        ]
    , connects1 (findWithDefault Empty o m) d
    , connects1 (findWithDefault Empty p m) (opposite d)
    ]

findPipe :: Grid2D Tile -> Map Point2D Integer
findPipe g = fst . execState go $ (singleton s 0, [s])
    where
        (s, _) = head . filter ((== Start) . snd) . toList $ g
        go = do
            (d, w) <- get
            if null w then
                return ()
            else do
                let c = head w
                let cc = findWithDefault 0 c d
                let ucs = [k | k <- connections g c, notMember k d]
                put (foldl (\m k -> insertWith min k (cc + 1) m) d ucs, (tail w) ++ ucs)
                go

solveA :: Grid2D Tile -> Integer
solveA = maximum . map snd . toList . findPipe

solveB :: Grid2D Tile -> Integer
solveB g
    = toInteger
    . length
    . filter (\k -> notMember k pipe && crossings k `mod` 2 == 1)
    . keys
    $ g
    where
        pipe = findPipe g
        crossings p@(x, y)
            | x < 0 = 0 :: Integer
            | member p pipe && connects1 (findWithDefault Empty p g) North = 1 + (crossings (x - 1, y))
            | otherwise = crossings (x - 1, y)

solution :: Day
solution = (
        show . solveA . readGrid2DWith readTile,
        show . solveB . readGrid2DWith readTile
    )
