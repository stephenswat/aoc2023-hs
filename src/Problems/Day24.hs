{-# LANGUAGE BlockArguments #-}

module Problems.Day24 (solution) where

import Control.Monad (forM)
import Data.SBV (AlgReal, constrain, solve, fromCV, getModelDictionary, sat, (.==), (.>=), sReal_, sReals, (%))
import Data.Map (lookup)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec (sepEndBy1, newline, string)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

type Hailstone = ((Integer, Integer, Integer), (Integer, Integer, Integer))

input :: AocInput () [Hailstone]
input = sepEndBy1 hailstone newline
    where
        hailstone = do
            px <- integer
            _ <- string ", "
            py <- integer
            _ <- string ", "
            pz <- integer
            _ <- string " @ "
            vx <- integer
            _ <- string ", "
            vy <- integer
            _ <- string ", "
            vz <- integer
            return ((px, py, pz), (vx, vy, vz))

solveA :: [Hailstone] -> Integer
solveA h
    = toInteger
    . length
    . filter id
    $ [valid x y | (i, x) <- zip ([0..] :: [Integer]) h, (j, y) <- zip ([0..] :: [Integer]) h, i < j]
    where
        valid ((px1, py1, _), (vx1, vy1, _)) ((px2, py2, _), (vx2, vy2, _))
            | d == 0 = False
            | ix >= 200000000000000 && ix <= 400000000000000 &&
              iy >= 200000000000000 && iy <= 400000000000000 &&
              t1 >= 0 && t2 >= 0 = True
            | otherwise = False
            where
                c1 = py1 * vx1 - px1 * vy1
                c2 = py2 * vx2 - px2 * vy2

                d = vy1 * (-vx2) - vy2 * (-vx1)

                ix = ((-vx1) * c2 - (-vx2) * c1) % d
                iy = (vy2 * c1 - vy1 * c2) % d

                t1 = (iy - toRational py1) / toRational vy1
                t2 = (iy - toRational py2) / toRational vy2

solveB :: [Hailstone] ->  AlgReal
solveB h =
    (fromCV . fromJust . Data.Map.lookup "px" $ d) +
    (fromCV . fromJust . Data.Map.lookup "py" $ d) +
    (fromCV . fromJust . Data.Map.lookup "pz" $ d)
    where
        d = getModelDictionary . unsafePerformIO . sat $ p
        p = do
            [px, py, pz, vx, vy, vz] <- sReals ["px", "py", "pz", "vx", "vy", "vz"]

            cs <- forM h (\((px', py', pz'), (vx', vy', vz')) -> do
                t <- sReal_
                constrain (t .>= 0)
                return
                    [ (fromInteger px') + t * (fromInteger vx') .== px + t * vx
                    , (fromInteger py') + t * (fromInteger vy') .== py + t * vy
                    , (fromInteger pz') + t * (fromInteger vz') .== pz + t * vz
                    ])

            solve (concat cs)

solution :: Day
solution = (
        show . solveA . aocParse input (),
        show . solveB . aocParse input ()
    )
