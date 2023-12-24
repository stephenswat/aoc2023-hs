module Problems.Day22 (solution) where

import Text.Parsec (sepEndBy1, newline, char)
import Control.Lens ((^.), (-~), (&), _1, _3)
import Data.Map as M (Map, empty, notMember, insert, elems, lookup, insertWith, toList)
import Data.List (nub, sortBy)
import Data.Set as S (Set, size, empty, union, fromList, singleton, isSubsetOf)
import Data.Function (on)
import Data.Maybe (fromJust)

import Common.Geometry (Point3D, Grid3D)
import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

type Block = (Point3D, Point3D)

input :: AocInput () [Block]
input = sepEndBy1 block newline
    where
        p3 = do
            x <- integer
            _ <- char ','
            y <- integer
            _ <- char ','
            z <- integer
            return (x, y, z)
        block = do
            f <- p3
            _ <- char '~'
            t <- p3
            return (f, t)

simulate :: [Block] -> Grid3D Integer
simulate bs = foldl simulate1 M.empty (zip [0..] (sortBy (compare `on` (\i -> i ^. _1 ^. _3)) bs))
    where
        materialize ((x1, y1, z1), (x2, y2, z2)) = [(x, y, z) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]
        simulate1m m ps
            | and . map (\i -> notMember i m && (i ^. _3) >= 1) $ nps = simulate1m m nps
            | otherwise = ps
            where
                nps = map (& _3 -~ 1) ps
        simulate1 m (n, b) = foldl (\m' p -> (insert p n m')) m (simulate1m m (materialize b))

supportMap :: Grid3D Integer -> Map Integer (Set Integer)
supportMap g = foldl (\m (i, l) -> insertWith union i l m) M.empty . map below . toList $ g
    where
        below ((x, y, z), i)
            | z == 1 = (i, singleton (-1))
            | Just k <- M.lookup (x, y, z - 1) g = if k /= i then (i, singleton k) else (i, S.empty)
            | otherwise = (i, S.empty)

solveA :: Grid3D Integer -> Integer
solveA g = toInteger . length . filter dis $ blockNums
    where
        blockNums = nub . elems $ g
        sbm = supportMap g
        dis n = and . map (\i -> (singleton n) /= (fromJust . M.lookup i $ sbm)) . filter (/= n) $ blockNums

solveB :: Grid3D Integer -> Integer
solveB g = sum . map ((+ (- 1)) . toInteger . size . wouldFall . singleton) $ blockNums
    where
        blockNums = nub . elems $ g
        sbm = supportMap g
        wouldFall s
            | s == ns = s
            | otherwise = wouldFall ns
            where
                ns = union s . fromList . filter (\i -> isSubsetOf (fromJust . M.lookup i $ sbm) s) $ blockNums

solution :: Day
solution = (
        show . solveA . simulate . aocParse input (),
        show . solveB . simulate . aocParse input ()
    )
