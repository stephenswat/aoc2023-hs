import System.Environment

import Common.Solution (Day)

import qualified Problems.Day01
import qualified Problems.Day02
import qualified Problems.Day03
import qualified Problems.Day04
import qualified Problems.Day05
import qualified Problems.Day06
import qualified Problems.Day07
import qualified Problems.Day08
import qualified Problems.Day09
import qualified Problems.Day10
import qualified Problems.Day11
import qualified Problems.Day12
import qualified Problems.Day13
import qualified Problems.Day14
import qualified Problems.Day15
import qualified Problems.Day16
import qualified Problems.Day17
import qualified Problems.Day18
import qualified Problems.Day19
import qualified Problems.Day20
import qualified Problems.Day21
import qualified Problems.Day22
import qualified Problems.Day23
import qualified Problems.Day24
import qualified Problems.Day25

solutions :: [Day]
solutions =
    [ Problems.Day01.solution
    , Problems.Day02.solution
    , Problems.Day03.solution
    , Problems.Day04.solution
    , Problems.Day05.solution
    , Problems.Day06.solution
    , Problems.Day07.solution
    , Problems.Day08.solution
    , Problems.Day09.solution
    , Problems.Day10.solution
    , Problems.Day11.solution
    , Problems.Day12.solution
    , Problems.Day13.solution
    , Problems.Day14.solution
    , Problems.Day15.solution
    , Problems.Day16.solution
    , Problems.Day17.solution
    , Problems.Day18.solution
    , Problems.Day19.solution
    , Problems.Day20.solution
    , Problems.Day21.solution
    , Problems.Day22.solution
    , Problems.Day23.solution
    , Problems.Day24.solution
    , Problems.Day25.solution
    ]

main :: IO ()
main = do {
    args <- getArgs;
    let
        dayn = read (args !! 0) :: Int
        sols = solutions !! (dayn - 1)
        fnam = args !! 1
    in do {
        putStrLn ("Solutions for day " ++ show dayn);
        file <- readFile fnam;
        let (solA, solB) = sols in do {
            putStrLn . ("Problem A: " ++) . solA $ file;
            putStrLn . ("Problem B: " ++) . solB $ file;
        }
    }
}
