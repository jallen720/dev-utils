module DevUtils.FileSystem
   ( FileMoveOp (..)
   , createFile
   , validateFilesDontExist
   , dirFromPath
   , checkRemoveEmptySubdirs
   , moveFile
   , recursiveFileList
   , createFileMoveOps
   , replaceInFile
   , fileExtension ) where


import System.Directory
   ( createDirectoryIfMissing
   , doesFileExist
   , doesDirectoryExist
   , listDirectory
   , removeDirectory
   , renameFile )

import qualified System.IO.Strict as Strict (readFile)
import System.IO (writeFile)
import Data.List (intercalate)
import Data.List.Split (endBy)
import Control.Monad (filterM)
import Control.Monad.Extra (partitionM)

import DevUtils.Utils
   ( ReplaceOp (..)
   , subString
   , lastIndex
   , directify
   , extensionify
   , getReplaceOpChain )

import DevUtils.UI (emptyLine)


data FileMoveOp =
   FileMoveOp
      { fromFile :: String
      , toFile   :: String }


createFile :: String -> String -> IO ()
createFile path content = do
   putStrLn $ "[CREATING] " ++ path
   createDirectoryIfMissing True (dirFromPath path)
   writeFile path content


moveFile :: FileMoveOp -> IO ()
moveFile (FileMoveOp fromFile toFile) = do
   putStrLn $ "[MOVING] " ++ fromFile ++ " -> " ++ toFile
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


recursiveFileList :: [String] -> String -> IO [String]
recursiveFileList extensions dir =
   listDirectory validDir
      >>= partitionM isFile . map (validDir ++)

      >>= \(files, dirs) ->
         if length dirs > 0
            then
               mapM (recursiveFileList extensions) dirs
                  >>= return . foldl (++) files

            else return files

      >>= return . validFiles

   where validDir = directify dir

         validFiles files =
            if length validExtensions == 0
               then files
               else filter hasValidFileExtension files

         hasValidFileExtension = (`elem` validExtensions) . fileExtension
         validExtensions = map extensionify extensions


fileExtension :: String -> String
fileExtension path =
   if '.' `elem` path
      then ('.':) . reverse . takeWhile (/= '.') $ reverse path
      else ""


isFile :: String -> IO Bool
isFile = doesFileExist


createFileMoveOps :: ([String] -> [String] -> [FileMoveOp])
createFileMoveOps = zipWith FileMoveOp


replaceInFile :: String -> [ReplaceOp] -> IO ()
replaceInFile filePath replaceOps = do
   printFileReplaceOps filePath replaceOps

   Strict.readFile filePath
      >>= writeFile filePath . getReplaceOpChain replaceOps


printFileReplaceOps :: String -> [ReplaceOp] -> IO ()
printFileReplaceOps filePath replaceOps = do
   putStrLn $ "[UPDATING] " ++ filePath ++ ":"
   mapM_ printReplaceOp replaceOps
   emptyLine


printReplaceOp :: ReplaceOp -> IO ()
printReplaceOp (ReplaceOp fromString toString) =
   putStrLn $ "    " ++ fromString ++ " -> " ++ toString
