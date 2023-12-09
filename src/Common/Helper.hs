module Common.Helper (runWhile, scanWhile, pairwise) where

runWhile :: (a -> (a, Bool)) -> a -> a
runWhile f i = let (r, c) = f i in if c then runWhile f r else r

scanWhile :: (a -> (a, Bool)) -> a -> [a]
scanWhile f i = let (r, c) = f i in if c then i:(scanWhile f r) else [r]

pairwise :: [a] -> [(a, a)]
pairwise (x:y:zs) = (x, y):(pairwise (y:zs))
pairwise _ = []
