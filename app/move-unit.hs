import System.Environment (getArgs)
import Data.List (nub)
import Data.String.Utils (replace)
import Control.Monad (mapM, filterM)
import DevUtils.Source (updateIncludes)
import DevUtils.Unit (unitFiles)
import DevUtils.UI (emptyLine)

import DevUtils.FileSystem
   ( FileMoveOp (..)
   , checkRemoveEmptySubdirs
   , dirFromPath
   , moveFile )


data UnitMoveArgs =
   UnitMoveArgs
      { unitName   :: String
      , fromSubdir :: String
      , toSubdir   :: String }


main :: IO ()
main = do
   emptyLine
   getUnitArgs >>= moveUnit
   emptyLine


getUnitArgs :: IO UnitMoveArgs
getUnitArgs = do
   args <- getArgs
   validateArgs args
   return
      UnitMoveArgs
         { unitName   = args !! 0
         , fromSubdir = args !! 1
         , toSubdir   = args !! 2 }


validateArgs :: [String] -> IO ()
validateArgs args =
   if isValidArgCount
      then return ()
      else error "usage: unit-name from-subdirectory to-subdirectory"

   where isValidArgCount = length args == 3


moveUnit :: UnitMoveArgs -> IO ()
moveUnit unitMoveArgs = do
   let
      uniqueSubdirs = nub . map fromFileSubdir
      fromFileSubdir (FileMoveOp fromFile _) = dirFromPath fromFile

   unitMoveMap <- getUnitMoveMap unitMoveArgs
   updateIncludes unitMoveMap
   mapM_ moveFile unitMoveMap

   -- Since there can be several unit files per subdir we need to get a unique
   -- list of subdirs from the files being moved so we don't try to remove the
   -- same empty subdir more than once.
   mapM_ checkRemoveEmptySubdirs $ uniqueSubdirs unitMoveMap


getUnitMoveMap :: UnitMoveArgs -> IO [FileMoveOp]
getUnitMoveMap (UnitMoveArgs unitName fromSubdir toSubdir) = do
   fromFiles <- unitFiles unitName fromSubdir
   let toFiles = map (replace fromSubdir toSubdir) fromFiles
   return $ zipWith FileMoveOp fromFiles toFiles
