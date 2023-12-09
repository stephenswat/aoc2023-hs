module Problems.Day09 (solution) where

import Text.Parsec (sepEndBy1, newline, char)

import Common.Parse (AocInput, aocParse, integer)
import Common.Solution (Day)
import Common.Helper (pairwise)

input :: AocInput () [[Integer]]
input = sepEndBy1 (sepEndBy1 integer (char ' ')) newline

solve :: [Integer] -> [[Integer]]
solve xs
    | and . map (== 0) $ xs = [xs]
    | otherwise = xs:(solve . map (\(a, b) -> b - a) . pairwise $ xs)

solution :: Day
solution = (
        show . sum . map (foldl1 (+) . map last . solve) . aocParse input (),
        show . sum . map (foldr1 (-) . map head . solve) . aocParse input ()
    )
