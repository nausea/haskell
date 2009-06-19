import Control.Monad.Error.Class
import Data.Char (isHexDigit, digitToInt)

data ParseError = Err {location :: Int, reason :: String}

instance Error ParseError where
    noMsg    = Err 0 "Parse Error"
    strMsg s = Err 0 s

-- ParseMonad is an Error Monad,
-- because its type is Either ParseError,
-- and ParseError is instance of Error.
-- There is an instance (Error e) => Monad (Either e) in Control.Monad.Error
type ParseMonad = Either ParseError

parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHexDigit c idx =
    if isHexDigit c then
        return (toInteger (digitToInt c))
    else
        throwError (Err idx ("Invalid character '" ++ [c] ++ "'"))

parseHex :: String -> ParseMonad Integer
parseHex s = parseHex' s 0 1
    where parseHex' []     val _   = return val
          parseHex' (c:cs) val idx = do d <- parseHexDigit c idx
                                        parseHex' cs ((val*16) + d) (idx + 1)

toString :: Integer -> ParseMonad String
toString n = return $ show n

convert :: String -> String
convert s =
    let (Right str) = do {n <- parseHex s; toString n} `catchError` printError
    in str

    where printError e = return $
             "At index " ++ (show (location e)) ++ ":" ++ (reason e)

                              