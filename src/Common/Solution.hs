module Common.Solution where

type Solution = String -> String

type Day = (Solution, Solution)

notImplemented :: Solution
notImplemented = \_ -> "Not implemented."
