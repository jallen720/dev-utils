import System.Environment (getArgs)
import Data.List (intercalate, nub)
import Data.String.Utils (replace)
import Control.Monad (zipWithM)
import DevUtils.Unit (unitFiles)
import DevUtils.Utils (directify)

import DevUtils.FileSystem
   ( checkRemoveEmptySubdirs
   , dirFromPath
   , moveFile )


data UnitArgs =
   UnitArgs
      { unitName   :: String
      , fromSubdir :: String
      , toSubdir   :: String }


main :: IO ()
main = getUnitArgs >>= moveUnit


getUnitArgs :: IO UnitArgs
getUnitArgs = do
   args <- getArgs
   validateArgs args
   return
      UnitArgs
         { unitName   = args !! 0
         , fromSubdir = args !! 1
         , toSubdir   = args !! 2 }


validateArgs :: [String] -> IO ()
validateArgs args =
   if isValidArgCount
      then return ()
      else error "usage: unit-name from-subdir to-subdir"

   where isValidArgCount = length args == validArgCount
         validArgCount = 3


moveUnit :: UnitArgs -> IO ()
moveUnit unitArgs = do
   let
      moveUnitFile (fromFile, toFile) = moveFile fromFile toFile
      uniqueFileSubdirs = nub . map fileSubdir
      fileSubdir (fromFile, _) = dirFromPath fromFile

   files <- unitFileMoveMap unitArgs
   mapM_ moveUnitFile files

   -- Since there can be several unit files per subdir we need to get a unique
   -- list of subdirs from the files being moved so we don't try to remove the
   -- same empty subdir more than once.
   mapM_ checkRemoveEmptySubdirs $ uniqueFileSubdirs files


unitFileMoveMap :: UnitArgs -> IO [(String, String)]
unitFileMoveMap (UnitArgs unitName fromSubdir toSubdir) = do
   fromFiles <- unitFiles unitName fromSubdir
   let toFiles = map (replace fromSubdir toSubdir) fromFiles
   return $ zip fromFiles toFiles
