import System.Environment (getArgs)
import System.Directory (doesDirectoryExist)
import Data.String.Utils (replace)
import Control.Monad (filterM, foldM)
import DevUtils.Unit (unitFileRootDirs, unitFileExtensions)
import DevUtils.Utils (quote, directify)
import DevUtils.Source (moveSourceFiles)
import DevUtils.Console (validateArgs)

import DevUtils.FileSystem
   ( FileMoveOp (..)
   , createFileMoveOps
   , recursiveFileList )


data RenameSubdirArgs =
   RenameSubdirArgs
      { fromSubdir :: String
      , toSubdir   :: String }


main :: IO ()
main = getRenameSubdirArgs >>= renameSubdir


getRenameSubdirArgs :: IO RenameSubdirArgs
getRenameSubdirArgs = do
   args <- getArgs
   validateArgs args 2 [ "from-subdirectory", "to-subdirectory" ]

   return
      RenameSubdirArgs
         { fromSubdir = args !! 0
         , toSubdir   = args !! 1 }


renameSubdir :: RenameSubdirArgs -> IO ()
renameSubdir (RenameSubdirArgs fromSubdir toSubdir) = do
   subdirChanges <- getSubdirChanges fromSubdir toSubdir
   validateAnyFromSubdirsExist fromSubdir subdirChanges
   getFileMoveOps subdirChanges >>= moveSourceFiles


getFileMoveOps :: [(String, String)] -> IO [FileMoveOp]
getFileMoveOps subdirChanges = do
   let
      getFileChanges = foldM buildFileChanges ([], []) subdirChanges

      buildFileChanges (fromAcc, toAcc) (fromSubdir, toSubdir) = do
         fromFiles <- recursiveFileList unitFileExtensions fromSubdir
         let toFiles = map (replace fromSubdir toSubdir) fromFiles
         return (fromAcc ++ fromFiles, toAcc ++ toFiles)

   (fromFiles, toFiles) <- getFileChanges
   return $ createFileMoveOps fromFiles toFiles


getSubdirChanges :: String -> String -> IO [(String, String)]
getSubdirChanges fromSubdir toSubdir =
   filterM (doesDirectoryExist . fst) $ map createSubdirChange unitFileRootDirs
   where createSubdirChange rootDir =
            (rootDir ++ fromSubdir, rootDir ++ toSubdir)


validateAnyFromSubdirsExist :: String -> [(String, String)] -> IO ()
validateAnyFromSubdirsExist fromSubdir subdirChanges =
   if anyFromSubdirsExist
      then return ()
      else error errorMsg

   where anyFromSubdirsExist = length subdirChanges > 0

         errorMsg =
            "couldn't find subdirectory "
            ++ printFromSubdir
            ++ " in any of these root-directories:\n"
            ++ printRootDirs

         printFromSubdir = quote . directify $ fromSubdir
         printRootDirs = unlines . map ("    " ++) $ unitFileRootDirs

