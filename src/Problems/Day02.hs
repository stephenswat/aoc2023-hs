module Problems.Day02 (solution) where

import Text.Parsec (sepEndBy1, sepBy, newline, string, char, space, (<|>))

import Common.Solution (Day, notImplemented)
import Common.Parse (AocInput, aocParse, integer)

data Round = Round
    { red :: Integer
    , green :: Integer
    , blue :: Integer
    }
    deriving Show

instance Monoid Round where
    mempty = Round { red=0, green=0, blue=0 }
    mconcat
        (Round { red=r1, green=g1, blue=b1 })
        (Round { red=r2, green=g2, blue=b2 })
        = Round { red=r1 + r2, green=g1 + g2, blue = b1 + b2}

data Game = Game
    { id :: Integer
    , rounds :: [Round]
    }
    deriving Show

input :: AocInput () GameState
input = do
    _ <- string "Game "
    id <- integer
    _ <- string ": "
    rounds <- sepEndBy1 round (string "; ")
    return Game { id=id, rounds=rounds }
    where
        round = do
            subRounds <- sepEndBy1 subRound (string ", ")
            return fold subRounds

solution :: Day
solution = (
        show . aocParse input (),
        notImplemented
    )
