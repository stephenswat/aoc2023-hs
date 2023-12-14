module Problems.Day08 (solution) where

import Data.Maybe (fromJust)
import Data.Map (Map, fromList, lookup, keys)
import Text.Parsec (sepEndBy1, newline, string, many1, char, choice, letter)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse)

data Direction = DirLeft | DirRight deriving (Eq, Show)

type MoveMap = Map String (String, String)

input :: AocInput () ([Direction], MoveMap)
input = do
    i <- many1 (choice [char 'L', char 'R'])
    _ <- string "\n\n"
    j <- sepEndBy1 line newline
    return (map directionFromChar i, fromList j)
    where
        directionFromChar 'L' = DirLeft
        directionFromChar 'R' = DirRight
        directionFromChar _   = error "Invalid direction!"

        line = do
            s <- many1 letter
            _ <- string " = "
            _ <- char '('
            l <- many1 letter
            _ <- string ", "
            r <- many1 letter
            _ <- char ')'
            return (s, (l, r))

getNextPosition :: MoveMap -> String -> Direction -> String
getNextPosition m f d
    | d == DirLeft = l
    | otherwise = r
    where
        (l, r) = fromJust . Data.Map.lookup f $ m

solveA :: ([Direction], MoveMap) -> Integer
solveA (ds, mm) = go "AAA" . cycle $ ds
    where
        go s (i:is)
            | s == "ZZZ" = 0
            | otherwise = 1 + (go (getNextPosition mm s i) is)
        go _ _ = error "Empty list not accepted!"

findCycle :: MoveMap -> String -> [Direction] -> Integer
findCycle mm s ds = go s . cycle $ ds
    where
        go p (i:is)
            | last p == 'Z' = 0
            | otherwise = 1 + (go (getNextPosition mm p i) is)
        go _ _ = error "Empty list not accepted!"

solveB :: ([Direction], MoveMap) -> Integer
solveB (ds, mm) = foldl1 lcm [findCycle mm x ds | x <- keys mm, last x == 'A']

solution :: Day
solution = (
        show . solveA . aocParse input (),
        show . solveB . aocParse input ()
    )
