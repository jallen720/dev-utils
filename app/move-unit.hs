import System.Environment (getArgs)
import Data.List (intercalate, nub)
import Data.String.Utils (replace)
import Control.Monad (zipWithM)
import DevUtils.Unit (unitFiles)
import DevUtils.Utils (directify)

import DevUtils.FileSystem
   ( FileMoveOp (..)
   , checkRemoveEmptySubdirs
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
      uniqueFileSubdirs = nub . map fromFileSubdir
      fromFileSubdir (FileMoveOp fromFile _) = dirFromPath fromFile

   unitFileMoveMap <- getUnitFileMoveMap unitArgs
   mapM_ moveFile unitFileMoveMap

   -- Since there can be several unit files per subdir we need to get a unique
   -- list of subdirs from the files being moved so we don't try to remove the
   -- same empty subdir more than once.
   mapM_ checkRemoveEmptySubdirs $ uniqueFileSubdirs unitFileMoveMap


getUnitFileMoveMap :: UnitArgs -> IO [FileMoveOp]
getUnitFileMoveMap (UnitArgs unitName fromSubdir toSubdir) = do
   fromFiles <- unitFiles unitName fromSubdir
   let toFiles = map (replace fromSubdir toSubdir) fromFiles
   return $ zipWith FileMoveOp fromFiles toFiles
