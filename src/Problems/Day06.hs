module Problems.Day06 (solution) where

import Text.Parsec (sepEndBy1, string, char, many1)
import Data.List (singleton)
import Control.Arrow ((&&&))

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

input :: AocInput () [(Integer, Integer)]
input = do
    _ <- string "Time:"
    _ <- many1 (char ' ')
    i <- sepEndBy1 integer (many1 (char ' '))
    _ <- string "\nDistance:"
    _ <- many1 (char ' ')
    j <- sepEndBy1 integer (many1 (char ' '))
    return (zip i j)

solve :: (Integer, Integer) -> Integer
solve (t, r) = toInteger . length . filter (> r) $ [c * (t - c) | c <- [0..t]]

transformB :: [(Integer, Integer)] -> (Integer, Integer)
transformB = (f fst) &&& (f snd)
    where f g = read . concat . map show . g . unzip

solution :: Day
solution = (
        show . product . map solve . aocParse input (),
        show . product . map solve . singleton . transformB . aocParse input ()
    )
