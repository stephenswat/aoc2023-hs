module Problems.Day18 (solution) where

import Text.Parsec (sepEndBy1, newline, char, alphaNum, many1, string, count)
import Control.Applicative ((<|>))
import Numeric (readHex)

import Common.Parse (AocInput, aocParse, integer)
import Common.Solution (Day)
import Common.Cardinal (Direction (..), Rotation (..), translateN, getRotation)

type Line = (Direction, Integer)

inputA :: AocInput () [Line]
inputA = sepEndBy1 line newline
    where
        line = do
            d <- (char 'R' >> return East) <|> (char 'L' >> return West) <|>
                 (char 'U' >> return North) <|> (char 'D' >> return South)
            _ <- string " "
            n <- integer
            _ <- string " (#"
            _ <- many1 alphaNum
            _ <- string ")"
            return (d, n)

inputB :: AocInput () [Line]
inputB = sepEndBy1 line newline
    where
        line = do
            _ <- (char 'R') <|> (char 'L') <|> (char 'U') <|> (char 'D')
            _ <- string " "
            _ <- integer
            _ <- string " (#"
            c <- count 5 alphaNum
            d <- (char '0' >> return East) <|> (char '2' >> return West) <|>
                 (char '3' >> return North) <|> (char '1' >> return South)
            _ <- string ")"
            return (d, fst . head . readHex $ c)

score :: [Line] -> Integer
score i = (2 * area + change) `div` 4
    where
        toPolygon p [] = [p]
        toPolygon p ((d, n):ls) = p:(toPolygon (translateN d n p) ls)
        getArea ((x1, y1):l2@(x2, y2):ls) = (x1 * y2 - x2 * y1) + (getArea (l2:ls))
        getArea _ = 0
        getChange ((d1, n1):l2@(d2, _):ls) = (n1 - 1) * 2 + corners + getChange (l2:ls)
            where
                corners = case getRotation d1 d2 of
                    RotateLeft -> 1
                    RotateRight -> 3
        getChange _ = 0
        area = getArea . toPolygon (0, 0) $ i
        change = getChange (i ++ [head i])

solution :: Day
solution = (
        show . score . aocParse inputA (),
        show . score . aocParse inputB ()
    )
