module Problems.Day25 (solution) where

import Data.Graph.Inductive as G (Gr, Adj, Node, empty, insNodes, insEdges, undir, labEdges, labNodes, edges, edgeLabel, noNodes, match, (&))
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Data.List (nub)
import Data.Map as M (empty, insertWith, toList, fromList, lookup)
import Text.Parsec (sepEndBy1, sepBy1, newline, letter, many1, char, string)
import System.Random (StdGen, mkStdGen, uniformR)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse)

type Graph = Gr Integer Integer

input :: AocInput () Graph
input = do
    l <- sepEndBy1 line newline
    let nlist = nub . concat . map (\(x, xs) -> x:xs) $ l
    let nmap = M.fromList (zip nlist [0..])
    return (undir . insEdges [(fromJust . M.lookup i $ nmap, fromJust . M.lookup j $ nmap, 1) | (i, js) <- l, j <- js] . insNodes [(fromJust . M.lookup i $ nmap, 1) | i <- nlist] $ G.empty)
    where
        line = do
            n <- many1 letter
            _ <- string ": "
            m <- sepBy1 (many1 letter) (char ' ')
            return (n, m)

mergeAdj :: Node -> Node -> Adj Integer -> Adj Integer -> Adj Integer
mergeAdj n1 n2 a b = filter (\(_, n) -> n /= n1 && n /= n2) . map swap . toList $ (foldl ff (foldl ff M.empty a) b)
    where
        ff m (v, k) = insertWith (+) k v m

contract :: Graph -> Node -> Node -> Graph
contract g n1 n2 = (mergeAdj n1 n2 f1 f2, m1, l1 + l2, mergeAdj n1 n2 t1 t2) & r2
    where
        (mc1, r1) = match n1 g
        (mc2, r2) = match n2 r1
        (f1, m1, l1, t1) = fromJust mc1
        (f2, _, l2, t2) = fromJust mc2

karger :: StdGen -> Graph -> (StdGen, Graph)
karger r g
    | noNodes g == 2 = (nr, g)
    | noNodes g < 2 = error "Graph is too small!"
    | otherwise = karger nr (contract g n1 n2)
    where
        ne = edges g
        (c, nr) = uniformR (0, (length ne) - 1) r
        (n1, n2) = ne !! c

solveA :: StdGen -> Graph -> Integer
solveA r g
    | and . map (== 3) $ (map edgeLabel . labEdges $ ng) = product . map snd . labNodes $ ng
    | otherwise = solveA nr g
    where
        (nr, ng) = karger r g

solution :: Day
solution = (
        show . solveA (mkStdGen 2) . aocParse input (),
        \_ -> "Merry Christmas!"
    )
