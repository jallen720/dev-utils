module DevUtils.Source (moveSourceFiles) where


import qualified System.IO.Strict as Strict (readFile)

import Data.List
   ( nub
   , isPrefixOf
   , isInfixOf )

import DevUtils.Utils
   ( ReplaceOp (..)
   , quote
   , dropUntilEndOf )

import DevUtils.Unit (unitFileRootDirs, unitFileExtensions)

import DevUtils.FileSystem
   ( FileMoveOp (..)
   , checkRemoveEmptySubdirs
   , dirFromPath
   , moveFile
   , recursiveFileList
   , replaceInFile )


includeRoots =
   [ "include/"
   , "tests/include/" ]


moveSourceFiles :: [FileMoveOp] -> IO ()
moveSourceFiles fileMoveOps = do
   let
      uniqueSubdirs = nub . map fromFileSubdir
      fromFileSubdir (FileMoveOp fromFile _) = dirFromPath fromFile

   updateIncludes fileMoveOps
   mapM_ moveFile fileMoveOps

   -- Since there can be several unit files per subdir we need to get a unique
   -- list of subdirs from the files being moved so we don't try to remove the
   -- same empty subdir more than once.
   mapM_ checkRemoveEmptySubdirs $ uniqueSubdirs fileMoveOps


updateIncludes :: [FileMoveOp] -> IO ()
updateIncludes fileMoveOps =
   sourceFileReplaceOps >>= mapM_ applyIncludeReplaceOps

   where applyIncludeReplaceOps (sourceFile, replaceOps) =
            replaceInFile sourceFile replaceOps

         sourceFileReplaceOps =
            sourceFiles
               >>= mapM findIncludeReplaceOps
               >>= return . filter hasIncludeReplaceOps

         hasIncludeReplaceOps = (>0) . length . snd

         findIncludeReplaceOps sourceFile =
            Strict.readFile sourceFile
               >>= return . assignIncludeReplaceOp sourceFile

         assignIncludeReplaceOp sourceFile source =
            ( sourceFile
            , filter (isInSource source) includesToCheckFor )

         isInSource source (ReplaceOp fromInclude _) =
            isInfixOf fromInclude source

         includesToCheckFor =
            map getIncludeReplaceOp . filter isIncludeFile $ fileMoveOps


getIncludeReplaceOp :: FileMoveOp -> ReplaceOp
getIncludeReplaceOp (FileMoveOp fromInclude toInclude) =
   ReplaceOp
      (includeDirective fromInclude)
      (includeDirective toInclude)


isIncludeFile :: FileMoveOp -> Bool
isIncludeFile (FileMoveOp fromFile _) =
   any (flip isPrefixOf fromFile) includeRoots


includeDirective :: String -> String
includeDirective = ("#include " ++) . quote . dropUntilEndOf "include/"


sourceFiles :: IO [String]
sourceFiles =
   mapM (recursiveFileList unitFileExtensions) unitFileRootDirs
      >>= return . foldl1 (++)
