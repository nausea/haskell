import System (system)
import System.IO
import System.Directory (getHomeDirectory)
import Control.Exception (finally)

layouts = ["tr", "en_US"]

readInt :: FilePath -> IO Int
readInt file = do 
  fh <- openFile file ReadMode
  inp <- hGetLine fh
  hClose fh
  return $ read inp

writeInt :: FilePath -> Int -> IO ()
writeInt file x = do
  fh <- openFile file WriteMode
  hPutStr fh $ show x
  hClose fh

main = do 
  file <- getFile
  cur <- readInt file
  let next = (cur + 1) `mod` length layouts
  writeInt file next
  system $ "setxkbmap -option ctrl:nocaps -layout " ++ show (layouts !! next)
    where getFile = do 
            home <- getHomeDirectory
            return $ home ++ "/.layout"
