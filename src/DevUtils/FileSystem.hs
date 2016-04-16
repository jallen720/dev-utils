module DevUtils.FileSystem
   ( FileMoveOp (..)
   , createFile
   , validateFilesDontExist
   , dirFromPath
   , checkRemoveEmptySubdirs
   , moveFile ) where


import System.Directory
   ( createDirectoryIfMissing
   , doesFileExist
   , doesDirectoryExist
   , listDirectory
   , removeDirectory
   , renameFile )

import System.IO (writeFile)
import Data.List (intercalate)
import Data.List.Split (endBy)
import Control.Monad (filterM)

import DevUtils.Utils
   ( subString
   , lastIndex
   , directify )


data FileMoveOp =
   FileMoveOp
      { fromFile :: String
      , toFile   :: String }


createFile :: String -> String -> IO ()
createFile path content = do
   putStrLn $ "creating file " ++ show path
   createDirectoryIfMissing True (dirFromPath path)
   writeFile path content


moveFile :: FileMoveOp -> IO ()
moveFile (FileMoveOp fromFile toFile) = do
   putStrLn $ "moving \"" ++ fromFile ++ "\" -> \"" ++ toFile ++ "\""
   createDirectoryIfMissing True (dirFromPath toFile)
   renameFile fromFile toFile


validateFilesDontExist :: [String] -> IO ()
validateFilesDontExist paths =
   filterM doesFileExist paths >>= \existingFiles ->
      if length existingFiles > 0
         then error $ "\n" ++ (unlines . map errorMessage) existingFiles
         else return ()

      where errorMessage = ("    file \"" ++) . (++ "\" already exists")


dirFromPath :: String -> String
dirFromPath path = subString (lastIndex '/' path) path


checkRemoveEmptySubdirs :: String -> IO ()
checkRemoveEmptySubdirs subdirPath =
   if isOnlyRoot
      then return ()
      else continueCheck

   where isOnlyRoot = dirCount subdirPath == 1
         dirCount = length . endBy "/"
         continueCheck = doesDirectoryExist subdirPath >>= checkSubdirExists

         checkSubdirExists subdirExists =
            if subdirExists
               then listDirectory subdirPath >>= checkSubdirIsEmpty
               else error $
                  "trying to remove subdir \""
                  ++ subdirPath
                  ++ "\" that doesn't exist"

         checkSubdirIsEmpty subdirContents =
            if length subdirContents == 0
               then removeEmptySubdir
               else return ()

         removeEmptySubdir = do
            putStrLn $ "removing empty subdirectory \"" ++ subdirPath ++ "\""
            removeDirectory subdirPath
            checkRemoveEmptySubdirs nextSubdirPath

         nextSubdirPath = intercalate "/" . init . endBy "/" $ subdirPath
