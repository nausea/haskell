todoList :: [IO ()]
todoList = [putChar 'a',
            do putChar 'b'
               putChar 'c',
            do c <- getChar
               putChar c]

-- sequence_' :: [IO ()] -> IO ()
-- sequence_' []      = return ()
-- sequence_' (a:as)  = do a
--                          mySequence_ as

sequence_' :: [IO ()] -> IO ()
sequence_' = foldr (>>) (return ())

putStr' = sequence_' . map putChar

mapM' f = sequence_' . map f

