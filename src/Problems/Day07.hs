module Problems.Day07 (solution) where

import Data.Function (on)
import Data.Ord (comparing)
import Data.Map (Map, insertWith, empty, elems)
import Data.List (sortBy, elemIndex)
import Text.Parsec (sepEndBy1, newline, many1, char, choice)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

cardsA :: String
cardsA = "AKQJT98765432"

cardsB :: String
cardsB = "AKQT98765432J"

(.<>) :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
(.<>) f g a b
    | c == EQ = f a b
    | otherwise = c
    where
        c = g a b

handValue :: String -> String -> String -> Ordering
handValue c = foldl (\g n -> (compare `on` (f . (!! n))) .<> g) (\_ _ -> EQ) [0, 1, 2, 3, 4]
    where
        f v = elemIndex v c

handType :: String -> Integer
handType s
    | elem 5 (elems count) = 0
    | elem 4 (elems count) = 1
    | elem 3 (elems count) && elem 2 (elems count) = 2
    | elem 3 (elems count) = 3
    | elem 2 (elems metaCount) = 4
    | elem 2 (elems count) = 5
    | otherwise = 6
    where
        count = foldl (\m x -> insertWith (+) x 1 m) (empty :: Map Char Integer) s
        metaCount = foldl (\m x -> insertWith (+) x 1 m) (empty :: Map Integer Integer) (elems count)

camelOrder :: Bool -> String -> String -> Ordering
camelOrder j = handValue (if j then cardsB else cardsA) .<> (comparing (handType . (if j then findBest else id)))
    where
        allVariants "" = [""]
        allVariants ('J':xs) = [x:xs' | x <- cardsA, x /= 'J', xs' <- allVariants xs]
        allVariants (x:xs) = [x:xs' | xs' <- allVariants xs]
        findBest = head . sortBy (comparing handType) . allVariants

input :: AocInput () [(String, Integer)]
input = sepEndBy1 bet newline
    where
        bet = do
            h <- many1 . choice . map char $ cardsA
            _ <- char ' '
            b <- integer
            return (h, b)

score :: Bool -> [(String, Integer)] -> Integer
score j s = sum . map f . zip [1..] . sortBy (flip ((camelOrder j) `on` fst)) $ s
    where
        f (r, (_, b)) = r * b

solution :: Day
solution = (
        show . score False . aocParse input (),
        show . score True . aocParse input ()
    )
