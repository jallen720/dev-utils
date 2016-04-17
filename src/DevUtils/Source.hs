module DevUtils.Source (updateIncludes) where


import qualified System.IO.Strict as Strict (readFile)

import Data.List
   ( nub
   , isPrefixOf
   , isInfixOf )

import Data.String.Utils (replace)
import Control.Monad (mapM)
import DevUtils.UI (emptyLine)
import DevUtils.Utils (quote)
import DevUtils.Unit (unitFileRootDirs, unitFileExtensions)
import DevUtils.FileSystem (FileMoveOp (..), recursiveFileList)


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
   mapM (recursiveFileList extensions) rootDirs >>= return . foldl1 (++)
   where extensions = nub unitFileExtensions
         rootDirs = nub unitFileRootDirs


printFileChanges :: (String, [(String, String)]) -> IO ()
printFileChanges (sourceFile, includeDiffs) = do
   putStrLn $ "[UPDATING] " ++ sourceFile ++ ":"
   mapM_ printIncludeDiff includeDiffs
   emptyLine


printIncludeDiff :: (String, String) -> IO ()
printIncludeDiff (fromInclude, toInclude) =
   putStrLn $ "    " ++ fromInclude ++ " -> " ++ toInclude
