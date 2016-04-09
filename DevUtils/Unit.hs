module DevUtils.Unit (
   UnitInput,
   Unit,
   unitData,
   createUnit,
   createUnitFiles,
   associatedFiles)
where


import System.Directory (doesFileExist)
import Data.List (nub)
import DevUtils.FileSystem (createFile)

import DevUtils.Utils (
   get,
   keys)


type Name      = String
type Namespace = String
type SubDir    = String
type FileKeys  = [Char]
type UnitInput = (Name, Namespace, SubDir, FileKeys)


data Unit =
   Unit {
      name      :: Name,
      namespace :: Namespace,
      subDir    :: SubDir,
      fileKeys  :: FileKeys
   }


includeRootDir = "include/"
headerExtension = ".hpp"
templateImplExtension = ".ipp"
templateImplFileKey = 'i'


unitData = [
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
createUnit (name, namespace, subDir, fileKeys) =
   Unit name namespace directifiedSubDir uniqueFileKeys
   where directifiedSubDir =
            if isDirectified
               then subDir
               else subDir ++ "/"

         isDirectified = last subDir == '/'
         uniqueFileKeys = nub . filter isFileKey $ fileKeys
         isFileKey = (`elem` keys unitData)


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
associatedFile (Unit name _ subDir _) fileKey =
   createFilePath (get fileKey unitData)
   where createFilePath unitFileData =
            get "rootDir" unitFileData ++
            subDir ++
            name ++
            get "extension" unitFileData


headerSnippet :: Unit -> String
headerSnippet unit =
   unlines [
      "#pragma once",
      "",
      emptyNamespaceSnippet unit
   ]
   ++ checkTemplateInclude
   where checkTemplateInclude =
            if templateImplFileKey `elem` (fileKeys unit)
               then unlines [
                     "",
                     unitInclude unit templateImplPath
                  ]
               else []


templateImplSnippet :: Unit -> String
templateImplSnippet unit = unlines [emptyNamespaceSnippet unit]


sourceSnippet :: Unit -> String
sourceSnippet unit =
   unlines [
      unitInclude unit headerPath,
      "",
      emptyNamespaceSnippet unit
   ]


testSourceSnippet :: Unit -> String
testSourceSnippet unit =
   unlines [
      unitInclude unit headerPath,
      "",
      "#include <gtest/gtest.h>",
      "",
      namespaceSnippet testSnippet unit
   ]
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
includePath extension (Unit name _ subDir _) =
   subDir ++ name ++ extension
