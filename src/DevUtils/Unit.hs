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
import DevUtils.Template (getTemplate)

import DevUtils.Utils
   ( ReplaceOp (..)
   , get
   , keys
   , directify
   , extensionify )


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
      ),
      (
         'f',
         [ ("description" , "test fixture")
         , ("rootDir"     , "tests/include/")
         , ("extension"   , ".hpp") ]
      )
   ]


unitFileRootDirs = nub . map (directify . get "rootDir" . snd) $ unitFileData
unitFileExtensions = nub . map (get "extension" . snd) $ unitFileData


snippetGetters =
   [ (templateImplFileKey , getTemplateImplSnippet)
   , ('s'                 , getSourceSnippet)
   , ('f'                 , getTestFixtureSnippet) ]


createUnit :: UnitInput -> Unit
createUnit (name, namespace, subdir) = Unit name namespace $ directify subdir


createUnitFiles :: Unit -> [FileKey] -> IO ()
createUnitFiles unit fileKeys = do
   validateFilesDontExist $ associatedFiles unit fileKeys
   mapM_ (createUnitFile unit fileKeys) fileKeys


createUnitFile :: Unit -> [FileKey] -> FileKey -> IO ()
createUnitFile unit fileKeys fileKey = getContent >>= createFile unitFile
   where unitFile = associatedFile unit fileKey
         getContent = snippetGetter unit

         snippetGetter =
            case fileKey of
               'h' -> headerSnippetGetter
               't' -> testSourceSnippetGetter
               _   -> get fileKey snippetGetters

         headerSnippetGetter =
            if 'i' `elem` fileKeys
               then getTemplateHeaderSnippet
               else getHeaderSnippet

         testSourceSnippetGetter =
            if 'f' `elem` fileKeys
               then getTestSourceWithFixtureSnippet
               else getTestSourceSnippet


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
   where createFilePath unitData =
            get "rootDir" unitData ++
            getSubdir ++
            getFileName ++
            get "extension" unitData

         getSubdir =
            if fileKey == 'f'
               then subdir ++ "Fixtures/"
               else subdir

         getFileName =
            if fileKey == 'f'
               then name ++ "Test"
               else name


getHeaderSnippet :: Unit -> IO String
getHeaderSnippet unit =
   emptyNamespaceSnippet unit >>= \content ->
      getTemplate "header" [ ReplaceOp "CONTENT" content ]


getTemplateHeaderSnippet :: Unit -> IO String
getTemplateHeaderSnippet unit =
   getHeaderSnippet unit >>= \headerSnippet ->
      getTemplateIncludeSnippet >>= \templateIncludeSnippet ->
         return $ headerSnippet ++ templateIncludeSnippet

   where getTemplateIncludeSnippet =
            getTemplate "templateInclude"
               [ ReplaceOp "TEMPLATE_INCLUDE" templateInclude ]

         templateInclude = unitInclude unit templateImplPath


getTemplateImplSnippet :: Unit -> IO String
getTemplateImplSnippet unit =
   emptyNamespaceSnippet unit >>= addTrailingNewline


getSourceSnippet :: Unit -> IO String
getSourceSnippet unit =
   emptyNamespaceSnippet unit >>= \content ->
      getTemplate "source" $ getSourceReplaceOps unit content


getTestSourceSnippet :: Unit -> IO String
getTestSourceSnippet unit =
   getTestSnippet unit False >>= \testSnippet ->
      getNamespaceSnippet testSnippet unit >>= \content ->
         getTemplate "testSource" $ getSourceReplaceOps unit content


getTestSourceWithFixtureSnippet :: Unit -> IO String
getTestSourceWithFixtureSnippet unit =
   getTestSnippet unit True >>= \testSnippet ->
      getNamespaceSnippet testSnippet unit >>= \content ->
         getTemplate "testSourceWithFixture" $
            getSourceReplaceOps unit content ++
            [ ReplaceOp "FIXTURE_INCLUDE" fixtureInclude ]

   where fixtureInclude = unitInclude unit fixturePath


getTestSnippet :: Unit -> Bool -> IO String
getTestSnippet (Unit name _ _) withFixture =
   getTemplate templateName [ ReplaceOp "NAME" name ] >>= stripTrailingNewline
   where templateName =
            if withFixture
               then "testWithFixture"
               else "test"


getTestFixtureSnippet :: Unit -> IO String
getTestFixtureSnippet unit =
   getTestFixtureClassSnippet unit >>= \testFixtureClassSnippet ->
      getNamespaceSnippet testFixtureClassSnippet unit >>= \content ->
         getTemplate "testFixture" [ ReplaceOp "CONTENT" content]


getTestFixtureClassSnippet :: Unit -> IO String
getTestFixtureClassSnippet (Unit name _ _) =
   getTemplate "testFixtureClass" [ ReplaceOp "NAME" name ]
       >>= stripTrailingNewline


emptyNamespaceSnippet :: (Unit -> IO String)
emptyNamespaceSnippet = getNamespaceSnippet ""


getNamespaceSnippet :: String -> Unit -> IO String
getNamespaceSnippet content (Unit _ namespace _) =
   getTemplate "namespace"
      [ ReplaceOp "NAMESPACE" namespace
      , ReplaceOp "CONTENT" content ]
      >>= stripTrailingNewline


unitInclude :: Unit -> (Unit -> String) -> String
unitInclude unit pathGetter = "#include \"" ++ pathGetter unit ++ "\""


headerPath :: Unit -> String
headerPath = includePath headerExtension


templateImplPath :: Unit -> String
templateImplPath = includePath templateImplExtension


includePath :: String -> Unit -> String
includePath extension (Unit name _ subdir) =
   validSubdir ++ name ++ validExtension
   where validSubdir = directify subdir
         validExtension = extensionify extension


fixturePath :: Unit -> String
fixturePath (Unit name _ subdir) =
   validSubdir ++ "Fixtures/" ++ name ++ "Test" ++ headerExtension
   where validSubdir = directify subdir


getSourceReplaceOps :: Unit -> String -> [ReplaceOp]
getSourceReplaceOps unit content =
   [ ReplaceOp "HEADER_INCLUDE" $ unitInclude unit headerPath
   , ReplaceOp "CONTENT" content ]


addTrailingNewline :: (String -> IO String)
addTrailingNewline = return . (++ "\n")


stripTrailingNewline :: (String -> IO String)
stripTrailingNewline = return . init
