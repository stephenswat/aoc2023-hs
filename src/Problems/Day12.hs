module Problems.Day12 (solution) where

import Control.Applicative ((<|>))
import Control.Monad.State.Lazy (State, evalState, get, modify)
import Data.List (intercalate)
import Data.Map (Map, empty, lookup, insert)
import Data.Maybe (isJust, fromJust)
import Text.Parsec (sepEndBy1, newline, char, many1, sepBy1)

import Common.Parse (AocInput, aocParse, integer)
import Common.Solution (Day)

input :: AocInput () [([Maybe Bool], [Integer])]
input = sepEndBy1 line newline
    where
        spring =
            (char '.' >> return (Just False)) <|>
            (char '#' >> return (Just True)) <|>
            (char '?' >> return Nothing)
        line = do
            h <- many1 spring
            _ <- char ' '
            b <- sepBy1 integer (char ',')
            return (h, b)

solutions :: [Integer] -> [Maybe Bool] -> Integer
solutions ci bi = evalState (go ci False bi) (empty)
    where
        go' :: [Integer] -> Bool -> [Maybe Bool] -> State (Map ([Integer], Bool, [Maybe Bool]) Integer) Integer
        go' cs bb bs = do
            m <- get
            let k = (cs, bb, bs)
            let l = Data.Map.lookup k m
            if isJust l then
                return (fromJust l)
            else do
                v <- go cs bb bs
                modify (insert k v)
                return v

        go :: [Integer] -> Bool -> [Maybe Bool] -> State (Map ([Integer], Bool, [Maybe Bool]) Integer) Integer
        go [0] _ [] = return 1
        go [] _ [] = return 1
        go _ _ [] = return 0
        go [] False ((Just False):xs) = go' [] False xs
        go (c:cs) _ ((Just True):xs)
            | c > 0 = go' ((c - 1):cs) True xs
            | otherwise = return 0
        go (c:cs) b ((Just False):xs)
            | not b && q = go' (c:cs) False xs
            | b && c == 0 && q = go' cs False xs
            | otherwise = return 0
            where
                q = (toInteger . length $ [i | i <- xs, i == Nothing || i == Just True]) >= (sum (c:cs))
        go cs b (Nothing:xs) = do
            q1 <- (go' cs b ((Just True):xs))
            q2 <- (go' cs b ((Just False):xs))
            return (q1 + q2)
        go _ _ _ = return 0

multiply :: Integer -> ([Maybe Bool], [Integer]) -> ([Maybe Bool], [Integer])
multiply n (bs, is) =
    ( intercalate [Nothing] [bs | _ <- [1..n] :: [Integer]]
    , concat [is | _ <- [1..n] :: [Integer]]
    )

solution :: Day
solution = (f 1, f 5)
    where
        f n = show . sum . map (\(bs, is) -> solutions is bs) . map (multiply n) . aocParse input ()
