module Problems.Day02 (solution) where

import Text.Parsec (sepEndBy1, sepBy1, newline, string, space, choice)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Round = Round
    { red :: Integer
    , green :: Integer
    , blue :: Integer
    }
    deriving Show

instance Semigroup Round where
    (<>)
        (Round { red=r1, green=g1, blue=b1 })
        (Round { red=r2, green=g2, blue=b2 })
        = Round { red=r1 + r2, green=g1 + g2, blue = b1 + b2}

instance Monoid Round where
    mempty = Round { red=0, green=0, blue=0 }

data Game = Game
    { num :: Integer
    , rounds :: [Round]
    }
    deriving Show

possibleA :: Game -> Bool
possibleA g = and [red r <= 12 && green r <= 13 && blue r <= 14 | r <- rounds g]

scoreB :: Game -> Integer
scoreB g = (red m) * (green m) * (blue m)
    where
        roundMax r1 r2 = Round
            { red = max (red r1) (red r2)
            , green = max (green r1) (green r2)
            , blue = max (blue r1) (blue r2)
            }
        m = foldr1 roundMax (rounds g)

input :: AocInput () [Game]
input = sepEndBy1 game newline
    where
        subRound = do
            n <- integer
            _ <- space
            s <- choice [string "red", string "green", string "blue"]
            return Round
                { red=if s == "red" then n else 0
                , green=if s == "green" then n else 0
                , blue=if s == "blue" then n else 0
                }
        gameRound = do
            subRounds <- sepBy1 subRound (string ", ")
            return (mconcat subRounds)
        game = do
            _ <- string "Game "
            n <- integer
            _ <- string ": "
            gameRounds <- sepBy1 gameRound (string "; ")
            return Game { num=n, rounds=gameRounds }

solution :: Day
solution = (
        show . sum . map num . filter possibleA . aocParse input (),
        show . sum . map scoreB . aocParse input ()
    )
