import System (system)
import System.Exit (ExitCode(..))
import System.IO
import System.Directory (getHomeDirectory)
import Control.Exception (finally)

layouts = ["tr", "en_US"]

withOpenFile :: FilePath -> (Handle -> IO a) -> IOMode -> IO a
withOpenFile path func mode = 
    do
      fh <- openFile path mode
      finally (func fh)
              (hClose fh)

writeInt :: FilePath -> Int -> IO ()
writeInt path int = withOpenFile path (\h -> hPutStr h $ show int) WriteMode

readInt :: FilePath -> IO Int
readInt path = withOpenFile path (\h -> hGetLine h >>= return . read) ReadMode

substituteHome :: FilePath -> IO FilePath
substituteHome ('~':'/':path) = getHomeDirectory >>= \x ->
                                return $ x ++ "/" ++ path
substituteHome path = return path

currLayoutIndex :: FilePath -> IO Int
currLayoutIndex layoutFile = readInt layoutFile
      
nextLayoutIndex :: FilePath -> IO Int
nextLayoutIndex layoutFile =
    do
      curr <- currLayoutIndex layoutFile
      let next = (curr + 1) `mod` length layouts
      writeInt layoutFile next
      return next

setLayout :: Int -> IO ExitCode
setLayout layoutIndex = system $ "setxkbmap -option ctrl:nocaps -layout " ++
                           show (layouts !! layoutIndex)

main =
    do 
      path <- substituteHome file
      next <- nextLayoutIndex path
      setLayout next
    where file = "~/.layout"
