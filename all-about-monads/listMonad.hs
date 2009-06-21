import Data.Char
    ( isDigit
    , isHexDigit
    , isAlpha
    , digitToInt)

import Monad

data Parsed = Digit Integer | Hex Integer | Word String
            deriving Show

parseHexDigit :: Parsed -> Char -> [Parsed]
parseHexDigit (Hex n) c
    | isHexDigit c = return $ Hex $  (n*16) + toInteger (digitToInt c)
    | otherwise    = mzero
parseHexDigit _ _  = mzero

parseDigit :: Parsed -> Char -> [Parsed]
parseDigit (Digit n) c
    | isDigit c = return $ Digit $  (n*10) + toInteger (digitToInt c)
    | otherwise = mzero
parseDigit _ _  = mzero

parseWord :: Parsed -> Char -> [Parsed]
parseWord (Word s) c
    | isAlpha c = return $ Word (s ++ [c])
    | otherwise = mzero
parseWord _ _   = mzero

parse :: Parsed -> Char -> [Parsed]
parse p c = parseHexDigit p c `mplus` parseDigit p c `mplus` parseWord p c

parseArg :: String -> [Parsed]
parseArg s = do init <- return (Hex 0) `mplus` return (Digit 0)
                                       `mplus` return (Word "")
                foldM parse init s
