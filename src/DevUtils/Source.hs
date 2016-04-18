module DevUtils.Source (moveSourceFiles) where


import qualified System.IO.Strict as Strict (readFile)

import Data.List
   ( nub
   , isPrefixOf
   , isInfixOf )

import Data.String.Utils (replace)
import DevUtils.UI (emptyLine)
import DevUtils.Utils (quote)
import DevUtils.Unit (unitFileRootDirs, unitFileExtensions)

import DevUtils.FileSystem
   ( FileMoveOp (..)
   , checkRemoveEmptySubdirs
   , dirFromPath
   , moveFile
   , recursiveFileList )


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
   sourceFileChanges >>= mapM_ applyChanges

   where applyChanges changes@(sourceFile, includeDiffs) = do
            printFileChanges changes

            Strict.readFile sourceFile
            >>= writeFile sourceFile . updateOp includeDiffs

         updateOp = foldl buildUpdateOp id

         buildUpdateOp updateOps (fromInclude, toInclude) =
            updateOps . replace fromInclude toInclude

         sourceFileChanges =
            sourceFiles
            >>= mapM findIncludeDiffs
            >>= return . filter hasIncludeDiffs

         hasIncludeDiffs = (>0) . length . snd

         findIncludeDiffs sourceFile =
            Strict.readFile sourceFile >>= return . assignIncludeDiff sourceFile

         assignIncludeDiff sourceFile source =
            (sourceFile, filter (isInSource source) includesToCheckFor)

         isInSource source (include, _) = isInfixOf include source

         includesToCheckFor
            = map includeDirectiveDiff
            . filter isIncludeFile
            $ fileMoveOps

         includeDirectiveDiff (FileMoveOp fromFile toFile) =
            ( includeDirective fromFile
            , includeDirective toFile )

         includeDirective = ("#include " ++) . quote . includePath
         includePath = drop $ length "include/"
         isIncludeFile = isPrefixOf "include/" . fromFile


sourceFiles :: IO [String]
sourceFiles =
   mapM (recursiveFileList unitFileExtensions) unitFileRootDirs
   >>= return . foldl1 (++)


printFileChanges :: (String, [(String, String)]) -> IO ()
printFileChanges (sourceFile, includeDiffs) = do
   putStrLn $ "[UPDATING] " ++ sourceFile ++ ":"
   mapM_ printIncludeDiff includeDiffs
   emptyLine


printIncludeDiff :: (String, String) -> IO ()
printIncludeDiff (fromInclude, toInclude) =
   putStrLn $ "    " ++ fromInclude ++ " -> " ++ toInclude
