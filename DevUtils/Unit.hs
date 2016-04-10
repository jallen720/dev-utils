module DevUtils.Unit
   ( UnitInput
   , Unit
   , unitFileData
   , createUnit
   , createUnitFiles
   , associatedFiles ) where


import System.Directory (doesFileExist)
import Data.List (nub)
import DevUtils.FileSystem (createFile)
import DevUtils.Utils (get, keys)


type Name      = String
type Namespace = String
type Subdir    = String
type FileKeys  = [Char]
type UnitInput = (Name, Namespace, Subdir, FileKeys)


data Unit =
   Unit
      { name      :: Name
      , namespace :: Namespace
      , subdir    :: Subdir
      , fileKeys  :: FileKeys }


includeRootDir = "include/"
headerExtension = ".hpp"
templateImplExtension = ".ipp"
templateImplFileKey = 'i'


unitFileData = [
      ('h', [
         ("description" , "header"),
         ("rootDir"     , includeRootDir),
         ("extension"   , headerExtension)
      ]),
      (templateImplFileKey, [
         ("description" , "template implementation"),
         ("rootDir"     , includeRootDir),
         ("extension"   , templateImplExtension)
      ]),
      ('s', [
         ("description" , "source"),
         ("rootDir"     , "src/"),
         ("extension"   , ".cpp")
      ]),
      ('t', [
         ("description" , "test source"),
         ("rootDir"     , "tests/src/"),
         ("extension"   , ".cpp")
      ])
   ]


unitFileSnippets = [
      ('h', headerSnippet),
      (templateImplFileKey, templateImplSnippet),
      ('s', sourceSnippet),
      ('t', testSourceSnippet)
   ]


createUnit :: UnitInput -> Unit
createUnit (name, namespace, subdir, fileKeys) =
   Unit name namespace directifiedSubdir uniqueFileKeys
   where directifiedSubdir =
            if isDirectified
               then subdir
               else subdir ++ "/"

         isDirectified = last subdir == '/'
         uniqueFileKeys = nub . filter isFileKey $ fileKeys
         isFileKey = (`elem` keys unitFileData)


createUnitFiles :: Unit -> IO ()
createUnitFiles unit@(Unit _ _ _ fileKeys) = do
   mapM_ (validateUnitFileDoesntExist unit) fileKeys
   mapM_ (createUnitFile unit) fileKeys


validateUnitFileDoesntExist :: Unit -> Char -> IO ()
validateUnitFileDoesntExist unit fileKey = doesFileExist unitFile >>= validate
   where unitFile = associatedFile unit fileKey
         validate fileExists =
            if fileExists
               then error $ "unit file \"" ++ unitFile ++ "\" already exists"
               else return ()


createUnitFile :: Unit -> Char -> IO ()
createUnitFile unit fileKey = createFile unitFile content
   where unitFile = associatedFile unit fileKey
         content = fileSnippet unit
         fileSnippet = get fileKey unitFileSnippets


associatedFiles :: Unit -> [String]
associatedFiles unit@(Unit _ _ _ fileKeys) = map (associatedFile unit) fileKeys


associatedFile :: Unit -> Char -> String
associatedFile (Unit name _ subdir _) fileKey =
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
   ++ checkTemplateInclude

   where checkTemplateInclude =
            if templateImplFileKey `elem` (fileKeys unit)
               then "\n" ++ unitInclude unit templateImplPath ++ "\n"
               else ""


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


emptyNamespaceSnippet :: Unit -> String
emptyNamespaceSnippet = namespaceSnippet ""


namespaceSnippet :: String -> Unit -> String
namespaceSnippet content (Unit _ namespace _ _) =
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
includePath extension (Unit name _ subdir _) =
   subdir ++ name ++ extension
