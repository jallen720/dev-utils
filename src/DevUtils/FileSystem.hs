module DevUtils.FileSystem
   ( createFile
   , validateFilesDontExist
   , dirFromPath ) where


import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.IO (writeFile)
import Control.Monad (filterM)
import DevUtils.Utils (subString, lastIndex)


createFile :: String -> String -> IO ()
createFile path content = do
   putStrLn $ "creating file " ++ show path
   createDirectoryIfMissing True (dirFromPath path)
   writeFile path content


validateFilesDontExist :: [String] -> IO ()
validateFilesDontExist paths =
   filterM doesFileExist paths >>= \existingFiles ->
      if length existingFiles > 0
         then error $ "\n" ++ (unlines . map errorMessage) existingFiles
         else return ()

      where errorMessage = ("    file \"" ++) . (++ "\" already exists")


dirFromPath :: String -> String
dirFromPath path = subString (lastIndex '/' path) path
