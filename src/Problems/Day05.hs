module Problems.Day05 (solution) where

import Text.Parsec (sepEndBy1, newline, string, space, char, letter, many1)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Range = Range
    { start :: Integer
    , size :: Integer
    }
    deriving Show

data Map = Map
    { from :: Integer
    , to :: Integer
    , len :: Integer
    }
    deriving Show

type Input = ([Integer], [[Map]])

input :: AocInput () Input
input = do
    _ <- string "seeds: "
    s <- sepEndBy1 integer (char ' ')
    _ <- string "\n\n"
    m <- sepEndBy1 mpng newline
    return (s, m)
    where
        range = do
            i <- integer
            _ <- space
            j <- integer
            _ <- space
            k <- integer
            return Map { from=j, to=i, len=k }
        mpng = do
            _ <- many1 letter
            _ <- string "-to-"
            _ <- many1 letter
            _ <- string " map:\n"
            r <- sepEndBy1 range newline
            return r

overlapBefore :: Map -> Range -> Range
overlapBefore m i = Range { start=newStart, size=newEnd - newStart }
    where
        newStart = start i
        newEnd = min ((start i) + (size i)) (from m)

overlapAfter :: Map -> Range -> Range
overlapAfter m i = Range { start=newStart, size=newEnd - newStart }
    where
        newStart = max (start i) (from m + len m)
        newEnd = (start i) + (size i)

overlap :: Map -> Range -> Range
overlap m i = Range { start=newStart, size=newEnd - newStart }
    where
        offset = (to m) - (from m)
        newStart = (max (start i) (from m)) + offset
        newEnd = (min (start i + size i) (from m + len m)) + offset

mapping :: [[Map]] -> [Range] -> [Range]
mapping = foldl (\f g -> (multiMap g) . f) id
    where
        basicMap :: Map -> Range -> ([Range], [Range])
        basicMap m r = (c, u)
            where
                c = filter ((> 0) . size) [overlap m r]
                u = filter ((> 0) . size) [overlapBefore m r, overlapAfter m r]
        singleMap :: [Map] -> Range -> ([Range], [Range])
        singleMap [] r = ([], [r])
        singleMap (m:ms) r = (c ++ c', u')
            where
                (c, u) = basicMap m r
                q = map (singleMap ms) $ u
                c' = concat . map fst $ q
                u' = concat . map snd $ q
        multiMap :: [Map] -> [Range] -> [Range]
        multiMap ms rs = (concat . map fst $ q) ++ (concat . map snd $ q)
            where
                q = map (singleMap ms) $ rs

makeRange :: (Integer, Integer) -> Range
makeRange (a, b) = Range { start=a, size=b }

solveA :: Input -> Integer
solveA (s, m) = minimum . map start . mapping m . map makeRange . pairs $ s
    where
        pairs [] = []
        pairs (a:xs) = (a, 1):(pairs xs)

solveB :: Input -> Integer
solveB (s, m) = minimum . map start . mapping m . map makeRange . pairs $ s
    where
        pairs [] = []
        pairs (a:b:xs) = (a, b):(pairs xs)
        pairs _ = error "Uneven number!"

solution :: Day
solution = (
        show . solveA . aocParse input (),
        show . solveB . aocParse input ()
    )
