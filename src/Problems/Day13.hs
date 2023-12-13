module Problems.Day13 (solution) where

import Text.Parsec (sepEndBy1, many1, newline, char)
import Control.Applicative ((<|>))
import Data.Map (fromList, mapKeys, intersection, keys, adjust)

import Common.Solution (Day)
import Common.Geometry (Grid2D)
import Common.Parse (AocInput, aocParse)

data Mirror = Horizontal | Vertical deriving (Eq, Show)

input :: AocInput () [Grid2D Bool]
input = sepEndBy1 grid newline
    where
        tile = (char '#' >> return True) <|> (char '.' >> return False)
        line = many1 tile
        grid = do
            ls <- sepEndBy1 line (newline)
            let q = [((x, y), c) | (y, r) <- zip [0..] ls, (x, c) <- zip [0..] r]
            return (fromList q)

findMirrors :: Grid2D Bool -> [(Mirror, Integer)]
findMirrors g = filter (uncurry validMirror) $ (candH ++ candV)
    where
        (valX, valY) = unzip . keys $ g
        candH = [(Horizontal, x) | x <- [1..(maximum valX)]]
        candV = [(Vertical, y) | y <- [1..(maximum valY)]]
        validMirror m v = (intersection g k) == (intersection k g)
            where
                k = mapKeys kmf g
                kmf (x, y)
                    | m == Horizontal = (2 * v - x - 1, y)
                    | otherwise       = (x, 2 * v - y - 1)

score :: (Mirror, Integer) -> Integer
score (Horizontal, i) = i
score (Vertical, i) = 100 * i

solveA :: Grid2D Bool -> Integer
solveA
    = score
    . head
    . findMirrors

solveB :: Grid2D Bool -> Integer
solveB g
    = score
    . head
    . filter (/= oldMirror)
    . concat
    . map findMirrors
    . map (\k -> adjust not k g)
    . keys
    $ g
    where
        oldMirror = head . findMirrors $ g

solution :: Day
solution = (
        show . sum . map solveA . aocParse input (),
        show . sum . map solveB . aocParse input ()
    )
