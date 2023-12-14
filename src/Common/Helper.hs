module Common.Helper (runWhile, scanWhile, pairwise, iterateCyclic) where

import Data.Map (insert, lookup, empty)

runWhile :: (a -> (a, Bool)) -> a -> a
runWhile f i = let (r, c) = f i in if c then runWhile f r else r

scanWhile :: (a -> (a, Bool)) -> a -> [a]
scanWhile f i = let (r, c) = f i in if c then i:(scanWhile f r) else [r]

pairwise :: [a] -> [(a, a)]
pairwise (x:y:zs) = (x, y):(pairwise (y:zs))
pairwise _ = []

iterateCyclic :: Ord a => (a -> a) -> Integer -> a -> a
iterateCyclic f n = go 0 empty
    where
        go i m v
            | i == n = v
            | (Just k) <- l =
                if (n - i) >= (i - k)
                then go (i + ((n - i) `div` (i - k)) * (i - k)) m v
                else go (i + 1) m (f v)
            | otherwise = go (i + 1) (insert v i m) (f v)
            where
                l = Data.Map.lookup v m
