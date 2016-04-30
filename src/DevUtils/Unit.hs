module DevUtils.Unit
   ( UnitInput
   , FileKey
   , Unit
   , unitFileData
   , unitFileRootDirs
   , unitFileExtensions
   , createUnit
   , createUnitFiles
   , unitFiles
   , associatedFiles ) where


import System.Directory (doesFileExist)
import Control.Monad (filterM)
import Data.List (nub)
import DevUtils.FileSystem (createFile, validateFilesDontExist)

import DevUtils.Utils
   ( get
   , keys
   , directify )


type Name      = String
type Namespace = String
type Subdir    = String
type UnitInput = (Name, Namespace, Subdir)
type FileKey   = Char


data Unit =
   Unit
      { name      :: Name
      , namespace :: Namespace
      , subdir    :: Subdir }


includeRootDir = "include/"
headerExtension = ".hpp"
templateImplExtension = ".ipp"
templateImplFileKey = 'i'


unitFileData =
   [
      (
         'h',
         [ ("description" , "header")
         , ("rootDir"     , includeRootDir)
         , ("extension"   , headerExtension) ]
      ),
      (
         templateImplFileKey,
         [ ("description" , "template implementation")
         , ("rootDir"     , includeRootDir)
         , ("extension"   , templateImplExtension) ]
      ),
      (
         's',
         [ ("description" , "source")
         , ("rootDir"     , "src/")
         , ("extension"   , ".cpp") ]
      ),
      (
         't',
         [ ("description" , "test source")
         , ("rootDir"     , "tests/src/")
         , ("extension"   , ".cpp") ]
      )
   ]


unitFileRootDirs = nub . map (directify . get "rootDir" . snd) $ unitFileData
unitFileExtensions = nub . map (get "extension" . snd) $ unitFileData


snippetGenerators =
   [ (templateImplFileKey , templateImplSnippet)
   , ('s'                 , sourceSnippet)
   , ('t'                 , testSourceSnippet) ]


createUnit :: UnitInput -> Unit
createUnit (name, namespace, subdir) = Unit name namespace $ directify subdir


createUnitFiles :: Unit -> [FileKey] -> IO ()
createUnitFiles unit fileKeys = do
   validateFilesDontExist $ associatedFiles unit fileKeys
   mapM_ (createUnitFile unit fileKeys) fileKeys


createUnitFile :: Unit -> [FileKey] -> FileKey -> IO ()
createUnitFile unit fileKeys fileKey = createFile unitFile content
   where unitFile = associatedFile unit fileKey
         content = snippetGenerator unit

         snippetGenerator =
            if fileKey == 'h'
               then headerSnippetGenerator
               else get fileKey snippetGenerators

         headerSnippetGenerator =
            if 'i' `elem` fileKeys
               then templateHeaderSnippet
               else headerSnippet


unitFiles :: Name -> Subdir -> IO [String]
unitFiles name subdir = filterM doesFileExist allFiles
   where allFiles = associatedFiles unit allFileKeys
         unit = createUnit (name, "", subdir)
         allFileKeys = keys unitFileData


associatedFiles :: Unit -> [FileKey] -> [String]
associatedFiles unit fileKeys = map (associatedFile unit) fileKeys


associatedFile :: Unit -> FileKey -> String
associatedFile (Unit name _ subdir) fileKey =
   createFilePath $ get fileKey unitFileData
   where createFilePath unitFileData =
            get "rootDir" unitFileData ++
            subdir ++
            name ++
            get "extension" unitFileData


headerSnippet :: Unit -> String
headerSnippet unit =
   unlines
      [ "#pragma once"
      , ""
      , ""
      , emptyNamespaceSnippet unit ]


templateHeaderSnippet :: Unit -> String
templateHeaderSnippet unit = headerSnippet unit ++ templateInclude
   where templateInclude = "\n\n" ++ unitInclude unit templateImplPath ++ "\n"


templateImplSnippet :: Unit -> String
templateImplSnippet unit = emptyNamespaceSnippet unit ++ "\n"


sourceSnippet :: Unit -> String
sourceSnippet unit =
   unlines
      [ unitInclude unit headerPath
      , ""
      , ""
      , emptyNamespaceSnippet unit ]


testSourceSnippet :: Unit -> String
testSourceSnippet unit =
   unlines
      [ unitInclude unit headerPath
      , ""
      , "#include <gtest/gtest.h>"
      , ""
      , ""
      , namespaceSnippet testSnippet unit ]

   where testSnippet =
            "TEST(" ++ name unit ++ "Test, test) {\n" ++
            "\n" ++
            "}"


emptyNamespaceSnippet :: (Unit -> String)
emptyNamespaceSnippet = namespaceSnippet ""


namespaceSnippet :: String -> Unit -> String
namespaceSnippet content (Unit _ namespace _) =
   "namespace " ++ namespace ++ " {\n" ++
   "\n" ++
   "\n" ++
   content ++ "\n" ++
   "\n" ++
   "\n" ++
   "} // namespace " ++ namespace


unitInclude :: Unit -> (Unit -> String) -> String
unitInclude unit path = "#include \"" ++ path unit ++ "\""


headerPath :: Unit -> String
headerPath = includePath headerExtension


templateImplPath :: Unit -> String
templateImplPath = includePath templateImplExtension


includePath :: String -> Unit -> String
includePath extension (Unit name _ subdir) =
   subdir ++ name ++ extension
