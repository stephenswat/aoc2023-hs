module Problems.Day20 (solution) where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (State, execState, evalState, get, modify')
import Text.Parsec (sepBy1, sepEndBy1, newline, char, many1, string, letter)
import Data.Map (Map, fromList, empty, lookup, insert, elems, mapWithKey, filter, keys)
import Control.Lens ((%~), (+~), (.~), (&), _1, _2, _3, _4, _5, _6, _7)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse)

data Node
    = Broadcaster ![String]
    | FlipFlop !Bool ![String]
    | Conjunction !(Map String Bool) ![String]
    deriving (Show, Eq)

type System = Map String Node

type Signal = (String, String, Bool)

type RunState = State (System, [Signal], [Signal], Integer, Integer, Integer, [(String, Integer)])

targets :: Node -> [String]
targets (Broadcaster d) = d
targets (FlipFlop _ d) = d
targets (Conjunction _ d) = d

input :: AocInput () System
input = do
    n <- sepEndBy1 (flipflop <|> conjunction <|> broadcaster) newline
    let s = fromList n
    return (mapWithKey (mapF s) s)
    where
        mapF s k (Conjunction m d) = (Conjunction nm d)
            where
                nm = foldl (\m' k' -> insert k' False m') m (keys . Data.Map.filter (\n -> elem k (targets n)) $ s)
        mapF _ _ i = i
        flipflop = do
            _ <- char '%'
            n <- many1 letter
            _ <- string " -> "
            d <- sepBy1 (many1 letter) (string ", ")
            return (n, FlipFlop False d)
        conjunction = do
            _ <- char '&'
            n <- many1 letter
            _ <- string " -> "
            d <- sepBy1 (many1 letter) (string ", ")
            return (n, Conjunction empty d)
        broadcaster = do
            _ <- string "broadcaster -> "
            d <- sepBy1 (many1 letter) (string ", ")
            return ("broadcaster", Broadcaster d)

pressOnce :: RunState ()
pressOnce = do
    modify' (& _2 .~ [("button", "broadcaster", False)])
    modify' (& _3 .~ [])
    singleStep
    modify' (& _6 +~ 1)
    where
        singleStep :: RunState ()
        singleStep = do
            (s, t0, t1, _, _, n, _) <- get
            if null t0 then
                if null t1 then
                    return ()
                else do
                    modify' (& _2 .~ t1)
                    modify' (& _3 .~ [])
                    singleStep
            else do
                let (cf, ct, cp) = head t0

                modify' (& _2 %~ tail)

                if elem cf ["vn", "dr", "ln", "zx"] && cp then
                    modify' (& _7 %~ (++ [(cf, n)]))
                else
                    return ()

                if cp then
                    modify' (& _5 +~ 1)
                else
                    modify' (& _4 +~ 1)

                case Data.Map.lookup ct $ s of
                    Just (Broadcaster d) -> do
                        modify' (& _3 %~ (++ [(ct, i, cp) | i <- d]))
                    Just (FlipFlop b d) -> case cp of
                        True -> return ()
                        False -> do
                            modify' (& _1 %~ (insert ct (FlipFlop (not b) d)))
                            modify' (& _3 %~ (++ [(ct, i, not b) | i <- d]))
                    Just (Conjunction m d) -> do
                        let nm = insert cf cp m
                        let np = and . elems $ nm
                        modify' (& _1 %~ (insert ct (Conjunction nm d)))
                        modify' (& _3 %~ (++ [(ct, i, not np) | i <- d]))
                    Nothing -> return ()
                singleStep

solveA :: System -> Integer
solveA s' = evalState (pressNTimes 1000) (s', [], [], 0, 0, 0, [])
    where
        pressNTimes :: Integer -> RunState Integer
        pressNTimes n
            | n == 0 = do
                (_, _, _, l, h, _, _) <- get
                return (l * h)
            | otherwise = do
                pressOnce
                pressNTimes (n - 1)

solveB :: System -> Integer
solveB s' = foldl1 lcm [(head . drop 2 $ l) - (head . drop 1 $ l) | i <- preludes, let l = [n | (j, n) <- q', j == i]]
    where
        (_, _, _, _, _, _, q') = execState (pressUntilAllCyclesFound preludes) (s', [], [], 0, 0, 0, [])
        preludes = ["vn", "dr", "ln", "zx"]
        pressUntilAllCyclesFound :: [String] -> RunState [[Integer]]
        pressUntilAllCyclesFound s = do
            (_, _, _, _, _, _, q) <- get
            let ccls = [[n | (j, n) <- q, j == i] | i <- s]
            if (and [length i >= 3 | i <- ccls]) then do
                return ccls
            else do
                pressOnce
                pressUntilAllCyclesFound s

solution :: Day
solution = (
        show . solveA . aocParse input (),
        show . solveB . aocParse input ()
    )
