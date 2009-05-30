import IO

-- getChar' :: IO Char
-- getChar' = getChar `catch` (\e -> return '\n')

getChar' :: IO Char
getChar' = getChar `catch` eofHandler where
    eofHandler e = if isEOFError e then return '\n' else ioError e


getLine' :: IO String
getLine' = catch getLine'' (\e -> return ("Error" ++ show e))
    where getLine'' = do c <- getChar'
                         if c == '\n' then return ""
                                      else do l <- getLine'
                                              return (c:l)
