import System.Environment (getArgs)
import Data.String.Utils (replace)
import DevUtils.Source (moveSourceFiles)
import DevUtils.Unit (unitFiles)
import DevUtils.UI (emptyLine)
import DevUtils.FileSystem (FileMoveOp (..), createFileMoveOps)


data MoveUnitArgs =
   MoveUnitArgs
      { unitName   :: String
      , fromSubdir :: String
      , toSubdir   :: String }


main :: IO ()
main = do
   emptyLine
   getMoveUnitArgs >>= moveUnit
   emptyLine


getMoveUnitArgs :: IO MoveUnitArgs
getMoveUnitArgs = do
   args <- getArgs
   validateArgs args
   return
      MoveUnitArgs
         { unitName   = args !! 0
         , fromSubdir = args !! 1
         , toSubdir   = args !! 2 }


validateArgs :: [String] -> IO ()
validateArgs args =
   if isValidArgCount
      then return ()
      else error "usage: unit-name from-subdirectory to-subdirectory"

   where isValidArgCount = length args == 3


moveUnit :: MoveUnitArgs -> IO ()
moveUnit moveUnitArgs = getUnitFileMoveOps moveUnitArgs >>= moveSourceFiles


getUnitFileMoveOps :: MoveUnitArgs -> IO [FileMoveOp]
getUnitFileMoveOps (MoveUnitArgs unitName fromSubdir toSubdir) = do
   fromFiles <- unitFiles unitName fromSubdir
   let toFiles = map (replace fromSubdir toSubdir) fromFiles
   return $ createFileMoveOps fromFiles toFiles
