module DevUtils.Unit
   ( UnitInput
   , FileKey
   , Unit
   , unitFileData
   , createUnit
   , createUnitFiles
   , associatedFiles ) where


import System.Directory (doesFileExist)
import DevUtils.FileSystem (createFile)
import DevUtils.Utils (get)


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


snippetGenerators =
   [ (templateImplFileKey , templateImplSnippet)
   , ('s'                 , sourceSnippet)
   , ('t'                 , testSourceSnippet) ]


createUnit :: UnitInput -> Unit
createUnit (name, namespace, subdir) = Unit name namespace validSubdir
   where validSubdir =
            if isValidSubdir
               then subdir
               else subdir ++ "/"

         isValidSubdir = last subdir == '/'


createUnitFiles :: Unit -> [FileKey] -> IO ()
createUnitFiles unit fileKeys = do
   mapM_ (validateUnitFileDoesntExist unit) fileKeys
   mapM_ (createUnitFile unit fileKeys) fileKeys


validateUnitFileDoesntExist :: Unit -> FileKey -> IO ()
validateUnitFileDoesntExist unit fileKey = doesFileExist unitFile >>= validate
   where unitFile = associatedFile unit fileKey
         validate fileExists =
            if fileExists
               then error $ "unit file \"" ++ unitFile ++ "\" already exists"
               else return ()


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
      , emptyNamespaceSnippet unit ]


templateHeaderSnippet :: Unit -> String
templateHeaderSnippet unit = headerSnippet unit ++ templateInclude
   where templateInclude = "\n" ++ unitInclude unit templateImplPath ++ "\n"


templateImplSnippet :: Unit -> String
templateImplSnippet unit = emptyNamespaceSnippet unit ++ "\n"


sourceSnippet :: Unit -> String
sourceSnippet unit =
   unlines
      [ unitInclude unit headerPath
      , ""
      , emptyNamespaceSnippet unit ]


testSourceSnippet :: Unit -> String
testSourceSnippet unit =
   unlines
      [ unitInclude unit headerPath
      , ""
      , "#include <gtest/gtest.h>"
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
   content ++ "\n" ++
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
