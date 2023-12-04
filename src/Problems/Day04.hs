module Problems.Day04 (solution) where

import Data.Maybe (fromJust)
import Data.Map (Map, elems, keys, lookup, fromList, toList, empty, size, unionWith)
import Text.Parsec (sepEndBy1, newline, string, many1, char)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Card = Card
    { winners :: [Integer]
    , candidates :: [Integer]
    }
    deriving Show

input :: AocInput () (Map Integer Card)
input = do
    cards <- sepEndBy1 card newline
    return (fromList cards)
    where
        card = do
            _ <- string "Card"
            _ <- many1 (char ' ')
            n <- integer
            _ <- string ":"
            _ <- many1 (char ' ')
            w <- sepEndBy1 integer (many1 (char ' '))
            _ <- string "|"
            _ <- many1 (char ' ')
            c <- sepEndBy1 integer (many1 (char ' '))
            return (n, Card { winners=w, candidates=c })

countWins :: Card -> Integer
countWins c = toInteger . length $ [i | i <- candidates c, elem i (winners c)]

scoreA :: Card -> Integer
scoreA c
    | countWins c == 0 = 0
    | otherwise = 2 ^ ((countWins c) - 1)

scoreB :: Map Integer Card -> Integer
scoreB m = sum . elems . go . fromList $ [(k, 1) | k <- keys m]
    where
        newCards c
            | wins == 0 = empty
            | otherwise = fromList [(c + k, 1) | k <- [1..wins]]
            where
                oldCard = fromJust . Data.Map.lookup c $ m
                wins = countWins oldCard
        go s
            | size s == 0 = empty
            | otherwise = unionWith (+) s (go next)
            where
                ffun i (c, n) = unionWith (+) i (fmap (* n) . newCards $ c)
                next = foldl ffun empty (toList s)

solution :: Day
solution = (
        show . sum . map scoreA . elems . aocParse input (),
        show . scoreB . aocParse input ()
    )
