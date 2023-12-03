module Problems.Day01 (solution) where

import Control.Arrow ((&&&))

import Common.Solution (Day)

concatInts :: (Integer, Integer) -> Integer
concatInts (a, b) = 10 * a + b

readDigits :: Bool -> String -> [Integer]
readDigits _ [] = []
readDigits b    ('1':xs)                 = (1:readDigits b xs)
readDigits b    ('2':xs)                 = (2:readDigits b xs)
readDigits b    ('3':xs)                 = (3:readDigits b xs)
readDigits b    ('4':xs)                 = (4:readDigits b xs)
readDigits b    ('5':xs)                 = (5:readDigits b xs)
readDigits b    ('6':xs)                 = (6:readDigits b xs)
readDigits b    ('7':xs)                 = (7:readDigits b xs)
readDigits b    ('8':xs)                 = (8:readDigits b xs)
readDigits b    ('9':xs)                 = (9:readDigits b xs)
readDigits True ('o':'n':'e':xs)         = (1:readDigits True ('n':'e':xs))
readDigits True ('t':'w':'o':xs)         = (2:readDigits True ('w':'o':xs))
readDigits True ('t':'h':'r':'e':'e':xs) = (3:readDigits True ('h':'r':'e':'e':xs))
readDigits True ('f':'o':'u':'r':xs)     = (4:readDigits True ('o':'u':'r':xs))
readDigits True ('f':'i':'v':'e':xs)     = (5:readDigits True ('i':'v':'e':xs))
readDigits True ('s':'i':'x':xs)         = (6:readDigits True ('i':'x':xs))
readDigits True ('s':'e':'v':'e':'n':xs) = (7:readDigits True ('e':'v':'e':'n':xs))
readDigits True ('e':'i':'g':'h':'t':xs) = (8:readDigits True ('i':'g':'h':'t':xs))
readDigits True ('n':'i':'n':'e':xs)     = (9:readDigits True ('i':'n':'e':xs))
readDigits b (_:xs) = readDigits b xs

solution :: Day
solution = (f False, f True)
    where
        f b = show . sum . map (concatInts . (head &&& last) . readDigits b) . lines
