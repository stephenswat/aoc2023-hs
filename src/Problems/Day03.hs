module Problems.Day03 (solution) where

import Data.Char (isDigit)
import Data.Map (keys, findWithDefault, insert, lookup)
import Data.Maybe (catMaybes)
import Data.List (nub)

import Common.Solution (Day)
import Common.Geometry (Grid2D, Point2D, readGrid2DWith, neighbours8)

data Tile
    = Digit Char
    | Symbol Char
    | Number Point2D Integer
    | Empty
    deriving (Show, Eq)

mapReader :: Char -> Tile
mapReader c
    | isDigit c = Digit c
    | c == '.' = Empty
    | otherwise = Symbol c

tileIsSymbol :: Tile -> Bool
tileIsSymbol (Symbol _) = True
tileIsSymbol _ = False

getDigit :: Tile -> Maybe Char
getDigit (Digit c) = Just c
getDigit _ = Nothing

getNumber :: Tile -> Maybe Integer
getNumber (Number _ v) = Just v
getNumber _ = Nothing

fwd :: Grid2D Tile -> Point2D -> Tile
fwd g p = findWithDefault Empty p g

findNumberAt :: Grid2D Tile -> Point2D -> Maybe (Point2D, [Point2D], Integer)
findNumberAt g (x, y)
    | (Digit _) <- fwd g (x - 1, y) = Nothing
    | otherwise = Just (head $ rslt, rslt, read . catMaybes . map (getDigit . fwd g) $ rslt)
    where
        go :: Point2D -> [Point2D]
        go (x', y')
            | (Digit _) <- fwd g (x', y') = (x', y'):(go (x' + 1, y'))
            | otherwise = []
        rslt = go (x, y)

transformGrid :: Grid2D Tile -> Grid2D Tile
transformGrid g
    = foldl (\g' (k, p, v) -> foldl (\g'' p' -> insert p' (Number k v) g'') g' p) g
    . catMaybes
    $ [findNumberAt g k | k <- keys g]

candidatesA :: Grid2D Tile -> [Integer]
candidatesA g
    = catMaybes
    . map getNumber
    . nub
    . catMaybes
    . map ((flip Data.Map.lookup) g)
    . filter (or . map (tileIsSymbol . fwd g) . neighbours8)
    . keys
    $ g

candidatesB :: Grid2D Tile -> [Integer]
candidatesB g
    = map product
    . filter ((== 2) . length)
    . map (catMaybes . map getNumber . nub . map (fwd g) . neighbours8)
    $ cogs
    where
        cogs = filter ((== (Symbol '*')) . fwd g) . keys $ g

solution :: Day
solution = (f candidatesA, f candidatesB)
    where f g = show . sum . g . transformGrid . readGrid2DWith mapReader
