{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Problems.Day19 (solution) where

import Control.Applicative ((<|>))
import Control.Lens (makeLenses, (^.), _1, _2)
import Text.Parsec (sepBy1, sepEndBy1, newline, char, many1, string, letter, try, lookAhead)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromJust)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Instruction
    = Goto String
    | Accept
    deriving (Eq)

data Item = Item
    { _valueX :: (Integer, Integer)
    , _valueM :: (Integer, Integer)
    , _valueA :: (Integer, Integer)
    , _valueS :: (Integer, Integer)
    }

makeLenses ''Item

type Workflow = Item -> [(Item, Instruction)]

-- Yuck.
splitOn :: Char -> Char -> Integer -> Item -> (Item, Item)
splitOn 'x' '>' v i = (i { _valueX=(max (v + 1) (fst . _valueX $ i), snd . _valueX $ i) }, i { _valueX=(fst . _valueX $ i, min v (snd . _valueX $ i)) })
splitOn 'x' '<' v i = (i { _valueX=(fst . _valueX $ i, min (v - 1) (snd . _valueX $ i)) }, i { _valueX=(max v (fst . _valueX $ i), snd . _valueX $ i) })
splitOn 'm' '>' v i = (i { _valueM=(max (v + 1) (fst . _valueM $ i), snd . _valueM $ i) }, i { _valueM=(fst . _valueM $ i, min v (snd . _valueM $ i)) })
splitOn 'm' '<' v i = (i { _valueM=(fst . _valueM $ i, min (v - 1) (snd . _valueM $ i)) }, i { _valueM=(max v (fst . _valueM $ i), snd . _valueM $ i) })
splitOn 'a' '>' v i = (i { _valueA=(max (v + 1) (fst . _valueA $ i), snd . _valueA $ i) }, i { _valueA=(fst . _valueA $ i, min v (snd . _valueA $ i)) })
splitOn 'a' '<' v i = (i { _valueA=(fst . _valueA $ i, min (v - 1) (snd . _valueA $ i)) }, i { _valueA=(max v (fst . _valueA $ i), snd . _valueA $ i) })
splitOn 's' '>' v i = (i { _valueS=(max (v + 1) (fst . _valueS $ i), snd . _valueS $ i) }, i { _valueS=(fst . _valueS $ i, min v (snd . _valueS $ i)) })
splitOn 's' '<' v i = (i { _valueS=(fst . _valueS $ i, min (v - 1) (snd . _valueS $ i)) }, i { _valueS=(max v (fst . _valueS $ i), snd . _valueS $ i) })
splitOn _   _   _ _ = error "Invalid splitting!"

isValid :: Item -> Bool
isValid i = and [(i ^. a ^. _1) <= (i ^. a ^. _2) | a <- [valueX, valueM, valueA, valueS]]

input :: AocInput () (Map String Workflow, [Item])
input = do
    r <- sepEndBy1 workflow newline
    _ <- string "\n"
    i <- sepEndBy1 item newline
    return (fromList r, i)
    where
        buildWorkflow :: [Item -> [(Item, Maybe Instruction)]] -> Workflow
        buildWorkflow (w:ws) i = matched ++ unmatched
            where
                result = w i
                matched = [(k, v) | (k, Just v) <- result]
                unmatched = concat [buildWorkflow ws k | (k, Nothing) <- result]
        buildWorkflow _ _ = error "Empty rule"
        instruction = do
            r <- (char 'A' >> return (Just Accept)) <|>
                 (char 'R' >> return Nothing) <|>
                 (do n <- try (many1 letter); return (Just . Goto $ n))
            _ <- lookAhead (char ',' <|> char '}')
            return r
        instructionRule = do
            j <- instruction
            return (case j of { Nothing -> (\_ -> []); (Just m) -> (\k -> [(k, Just m)]) })
        comparisonRule = do
            projection <-
                (char 'x') <|>
                (char 'm') <|>
                (char 'a') <|>
                (char 's')
            operator <-
                (char '<') <|>
                (char '>')
            v <- integer
            _ <- char ':'
            j <- instruction
            let insTrue k = case j of
                    (Just Accept) -> [(k, (Just Accept))]
                    (Just (Goto s)) -> [(k, (Just (Goto s)))]
                    Nothing -> []
            return (\i -> let (t, f) = splitOn projection operator v i in filter (isValid . fst) ((insTrue t) ++ [(f, Nothing)]))
        workflow = do
            n <- many1 letter
            _ <- string "{"
            i <- sepBy1 ((try instructionRule) <|> (try comparisonRule)) (char ',')
            _ <- string "}"
            return (n, buildWorkflow i)
        item = do
            _ <- string "{x="
            x <- integer
            _ <- string ",m="
            m <- integer
            _ <- string ",a="
            a <- integer
            _ <- string ",s="
            s <- integer
            _ <- string "}"
            return Item { _valueX=(x, x), _valueM=(m, m), _valueA=(a, a), _valueS=(s, s) }

evaluate :: Map String Workflow -> Item -> [Item]
evaluate m = go "in"
    where
        go s i = concat [ case k of { Accept -> [j]; (Goto s') -> go s' j} | (j, k) <- fromJust (Data.Map.lookup s m) i ]

solveA :: (Map String Workflow, [Item]) -> Integer
solveA (m, is) = sum . map score . filter (not . null . evaluate m) $ is
    where
        score i = sum [i ^. a ^. _1 | a <- [valueX, valueM, valueA, valueS]]

solveB :: (Map String Workflow, [Item]) -> Integer
solveB (m, _) = sum . map rangeSize . evaluate m $ iItem
    where
        iItem = Item { _valueX=(1,4000), _valueM=(1,4000), _valueA=(1,4000), _valueS=(1,4000) }
        rangeSize i = product [(i ^. a ^. _2) - (i ^. a ^. _1) + 1 | a <- [valueX, valueM, valueA, valueS]]

solution :: Day
solution = (
        show . solveA . aocParse input (),
        show . solveB . aocParse input ()
    )
