module Problems.Day15 (solution) where

import Control.Applicative ((<|>))
import Text.Parsec (sepBy1, many1, alphaNum, char)
import Data.Char (ord)
import Data.Map (fromList, toList, adjust)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Lens
    = Remove String
    | Add String Integer
    deriving (Show, Eq)

inputA :: AocInput () [String]
inputA = sepBy1 (many1 (alphaNum <|> (char '-') <|> (char '='))) (char ',')

inputB :: AocInput () [Lens]
inputB = sepBy1 lens (char ',')
    where
        lens = do
            l <- many1 alphaNum
            (do _ <- char '-'; return (Remove l)) <|>
                (do _ <- char '='; n <- integer; return (Add l n))

hash :: String -> Integer
hash = foldl (\n c -> ((n + (toInteger . ord $ c)) * 17) `mod` 256) 0

solveA :: [String] -> Integer
solveA = sum . map hash

solveB :: [Lens] -> Integer
solveB = sum . map score . toList . foldl applyLens emptyMap
    where
        emptyMap = fromList [(x, []) | x <- [0..256]]
        score (k, v) = (k + 1) * sum (map (uncurry (*)) (zip [1..] (map snd v)))
        applyLens m (Remove s) = adjust (filter ((/= s) . fst)) (hash s) m
        applyLens m (Add s v) = adjust (addAdjustFun (s, v)) (hash s) m
        addAdjustFun (k, v) l
            | or [k == k' | (k', _) <- l] = [(k', if k' == k then v else v')| (k', v') <- l]
            | otherwise = l ++ [(k, v)]

solution :: Day
solution = (
        show . solveA . aocParse inputA (),
        show . solveB . aocParse inputB ()
    )
