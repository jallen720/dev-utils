import System.Environment (getArgs)
import qualified System.IO.Strict as Strict (readFile)
import Control.Monad (filterM)
import Data.List(isInfixOf, nub)
import DevUtils.UI (emptyLine)
import DevUtils.Console (validateArgs)
import DevUtils.Utils (directify, dropUntilEndOf)
import DevUtils.Unit (Unit (..), associatedFile)

import DevUtils.Source
   ( isIncludeFile
   , includeDirective
   , sourceFiles )


data UnitDependentsArgs =
   UnitDependentsArgs
      { unitName   :: String
      , unitSubdir :: String }


main :: IO ()
main = do
   let
      unitDependentsMessage unitDependents =
         if length unitDependents == 0
            then "no dependents\n"
            else unlines unitDependents

   emptyLine

   getUnitDependentsArgs
      >>= unitDependents
      >>= putStrLn . unitDependentsMessage


getUnitDependentsArgs :: IO UnitDependentsArgs
getUnitDependentsArgs = do
   args <- getArgs
   validateArgs args 2 [ "unit-name", "unit-subdirectory" ]

   return
      UnitDependentsArgs
         { unitName   = args !! 0
         , unitSubdir = args !! 1 }


unitDependents :: UnitDependentsArgs -> IO [String]
unitDependents (UnitDependentsArgs unitName unitSubdir) =
   sourceFiles
      >>= filterM isDependent
      >>= return . getDependentUnits

   where isDependent file =
            Strict.readFile file
               >>= return . isInfixOf unitIncludeDirective

         unitIncludeDirective = includeDirective unitHeader
         unitHeader = associatedFile unit 'h'
         unit = Unit unitName "" $ directify unitSubdir
         getDependentUnits = nub . filter (/= unitName) . map getFileUnitName

         getFileUnitName =
            reverse . takeWhile (/= '/') . reverse . takeWhile (/= '.')

