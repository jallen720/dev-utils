import System.Environment (getArgs)
import System.Directory (renameFile)
import Data.List (intercalate)
import Data.String.Utils (replace)
import Control.Monad (zipWithM)
import DevUtils.Unit (unitFiles)
import DevUtils.Utils (directify)


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
moveUnit unitArgs = unitFileMoveMap unitArgs >>= mapM_ moveUnitFile
   where moveUnitFile (fromFile, toFile) = renameFile fromFile toFile


unitFileMoveMap :: UnitArgs -> IO [(String, String)]
unitFileMoveMap (UnitArgs unitName fromSubdir toSubdir) = do
   fromFiles <- unitFiles unitName fromSubdir
   let toFiles = map (replace fromSubdir toSubdir) fromFiles
   return $ zip fromFiles toFiles
