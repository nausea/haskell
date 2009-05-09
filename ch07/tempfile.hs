import System.IO
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO.Error (catch)
import Control.Exception (finally)

main :: IO ()
main = withTempFile "mytemp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph =
    do
      putStrLn "Welcome to tempfile.hs"
      putStrLn $ "I have a temporary file at " ++ tempname
      
      -- Print what the initial pos
      pos <- hTell temph
      putStrLn $ "My initial pos is " ++ show pos
      
      -- Write some data to temp file
      let tempdata = show [1..10]
      putStrLn $ "Writing one line containing " ++
               show (length tempdata) ++ " bytes: " ++ tempdata
      hPutStrLn temph tempdata

      -- Print new pos
      pos <- hTell temph
      putStrLn $ "After writing, my new pos is " ++ show pos

      -- Seek to the beginning of the file and display the contents
      putStrLn "The file content is: "
      hSeek temph AbsoluteSeek 0
      c <- hGetContents temph
      putStrLn c

      -- Display as a Haskell literal
      putStrLn $ "Which could be expressed as this Haskell literal:"
      print c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
    do
      -- Open the temp file
      tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
      (tempfile, temph) <- openTempFile tempdir pattern

      -- Perform the function
      finally (func tempfile temph)
              (do hClose temph
                  removeFile tempfile)
