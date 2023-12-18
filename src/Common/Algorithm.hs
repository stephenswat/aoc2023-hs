module Common.Algorithm (dijkstra, bfs) where

import Data.PQueue.Prio.Min (null, insert, deleteFindMin, empty)
import Data.Set (Set, map, unions, member, insert, empty, notMember)
import Data.Map (insertWith, fromList)

dijkstra :: Ord a => (a -> Bool) -> (a -> [(a, Integer)]) -> [(a, Integer)] -> Maybe (a, Integer)
dijkstra f g i = go Data.Set.empty iQueue iMap
    where
        iQueue = foldl (\q (k, v) -> Data.PQueue.Prio.Min.insert v k q) Data.PQueue.Prio.Min.empty i
        iMap = Data.Map.fromList i
        go a x d
            | Data.PQueue.Prio.Min.null x = Nothing
            | f u = Just (u, w)
            | Data.Set.member u a = go na nx' nd
            | otherwise = go na nx nd
            where
                ((w, u), nx') = deleteFindMin x
                na = Data.Set.insert u a
                nb = [(p, w + nw) | (p, nw) <- g u, Data.Set.notMember p na]
                nx = foldl (\m (n, wt) -> Data.PQueue.Prio.Min.insert wt n m) nx' nb
                nd = foldl (\m (n, wt) -> Data.Map.insertWith min n wt m) d nb

bfs :: Ord c => (c -> Bool) -> (s -> s) -> (s -> c -> Set c) -> s -> Set c -> Integer
bfs a t f s p
    | any a p = 0
    | otherwise = 1 + (bfs a t f ns (unions . Data.Set.map (f ns) $ p))
    where
        ns = t s
