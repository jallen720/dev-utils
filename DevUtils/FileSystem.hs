module DevUtils.FileSystem (createFile) where


import System.Directory (createDirectoryIfMissing)
import System.IO (writeFile)

import DevUtils.Utils (
   subString,
   lastIndex)


createFile :: String -> String -> IO ()
createFile path content = do
   putStrLn $ "creating file " ++ show path
   createDirectoryIfMissing True (dirFromPath path)
   writeFile path content


dirFromPath :: String -> String
dirFromPath path = subString (lastIndex '/' path) path
