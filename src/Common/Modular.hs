module Common.Modular ((!!%)) where

(!!%) :: [a] -> Int -> a
(!!%) l i = l !! (i `mod` (length l))
