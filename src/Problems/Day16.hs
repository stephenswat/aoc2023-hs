module Problems.Day16 (solution) where

import Data.Map (notMember, findWithDefault, keys)
import Data.Set (Set, empty, size, insert, member, map)
import Control.Monad.State.Lazy (State, execState, get, modify)

import Common.Geometry (Grid2D, Point2D, readGrid2DWith)
import Common.Cardinal (Direction (..), translate, opposite)
import Common.Solution (Day)

data Tile
    = SplitterH
    | SplitterV
    | MirrorNW
    | MirrorNE
    | Empty

readTile :: Char -> Tile
readTile '-'  = SplitterH
readTile '|'  = SplitterV
readTile '\\' = MirrorNE
readTile '/'  = MirrorNW
readTile '.'  = Empty
readTile _    = error "Invalid tile to read"

trace :: Point2D -> Direction -> Grid2D Tile -> Set Point2D
trace p0 d0 g = Data.Set.map fst . execState (go p0 d0) $ empty
    where
        go :: Point2D -> Direction -> State (Set (Point2D, Direction)) ()
        go p d = do
            s <- get
            if Data.Set.member (p, d) s then
                return ()
            else if notMember p g then
                return ()
            else do
                modify (insert (p, d))
                case findWithDefault Empty p g of
                    SplitterH -> do
                        if d == South || d == North
                        then do
                            go (translate East p) East
                            go (translate West p) West
                        else go (translate d p) d
                    SplitterV -> do
                        if d == East || d == West
                        then do
                            go (translate North p) North
                            go (translate South p) South
                        else go (translate d p) d
                    MirrorNW -> case (opposite d) of
                        West -> go (translate North p) North
                        East -> go (translate South p) South
                        South -> go (translate East p) East
                        North -> go (translate West p) West
                    MirrorNE -> case (opposite d) of
                        West -> go (translate South p) South
                        East -> go (translate North p) North
                        South -> go (translate West p) West
                        North -> go (translate East p) East
                    Empty -> go (translate d p) d

solveB :: Grid2D Tile -> Integer
solveB g = maximum . fmap (toInteger . size . (\(p0, d0) -> trace p0 d0 g)) $ (iNorth ++ iSouth ++ iWest ++ iEast)
    where
        (valX, valY) = unzip . keys $ g
        iNorth = [((x, 0), South) | x <- [0..(maximum valX)]]
        iSouth = [((x, maximum valY), North) | x <- [0..(maximum valX)]]
        iWest = [((0, y), East) | y <- [0..(maximum valY)]]
        iEast = [((maximum valX, y), West) | y <- [0..(maximum valY)]]

solution :: Day
solution = (
        show . size . (trace (0, 0) East) . readGrid2DWith readTile,
        show . solveB . readGrid2DWith readTile
    )
