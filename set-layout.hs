import System (system)
import System.Exit (ExitCode(..))
import System.IO
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import Control.Exception (finally)

layouts = ["tr", "en_US"]

main =
    do
      args <- getArgs
      path <- substituteHome file
      case args of
        ["switch"] -> do next <- nextLayoutIndex path
                         setLayout next
        _          -> do current <- currentLayoutIndex path
                         setLayout current

    where file = "~/.layout"

currentLayoutIndex :: FilePath -> IO Int
currentLayoutIndex layoutFile = readInt layoutFile
      
nextLayoutIndex :: FilePath -> IO Int
nextLayoutIndex layoutFile =
    do
      curr <- currentLayoutIndex layoutFile
      let next = (curr + 1) `mod` length layouts
      writeInt layoutFile next
      return next

setLayout :: Int -> IO ExitCode
setLayout layoutIndex = system $ "setxkbmap -option ctrl:nocaps -layout " ++
                           show (layouts !! layoutIndex)

writeInt :: FilePath -> Int -> IO ()
writeInt path int = withOpenFile path (\h -> hPutStr h $ show int) WriteMode

readInt :: FilePath -> IO Int
readInt path = withOpenFile path (\h -> hGetLine h >>= return . read) ReadMode

withOpenFile :: FilePath -> (Handle -> IO a) -> IOMode -> IO a
withOpenFile path func mode = 
    do
      fh <- openFile path mode
      finally (func fh)
              (hClose fh)

substituteHome :: FilePath -> IO FilePath
substituteHome ('~':'/':path) = getHomeDirectory >>= \x ->
                                return $ x ++ "/" ++ path
substituteHome path = return path
