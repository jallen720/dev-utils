import System.Environment (getArgs)
import Data.List.Split (splitOn)
import DevUtils.Source (moveSourceFiles)
import DevUtils.Unit (unitFiles)
import DevUtils.UI (emptyLine)

import DevUtils.FileSystem
   ( FileMoveOp (..)
   , createFileMoveOps
   , fileExtension )

import DevUtils.Console (validateArgs)


data RenameUnitArgs =
   RenameUnitArgs
      { fromName :: String
      , toName   :: String
      , subdir   :: String }


main :: IO ()
main = do
   emptyLine
   getRenameUnitArgs >>= renameUnit
   emptyLine


getRenameUnitArgs :: IO RenameUnitArgs
getRenameUnitArgs = do
   args <- getArgs
   validateArgs args 3 [ "from-name", "to-name", "unit-subdirectory" ]

   return
      RenameUnitArgs
         { fromName = args !! 0
         , toName   = args !! 1
         , subdir   = args !! 2 }


renameUnit :: RenameUnitArgs -> IO ()
renameUnit moveUnitArgs =
   getUnitFileMoveOps moveUnitArgs >>= moveSourceFiles


getUnitFileMoveOps :: RenameUnitArgs -> IO [FileMoveOp]
getUnitFileMoveOps (RenameUnitArgs fromName toName subdir) = do
   fromFiles <- unitFiles fromName subdir
   let toFiles = map (renameUnitFile toName) fromFiles
   return $ createFileMoveOps fromFiles toFiles


renameUnitFile :: String -> String -> String
renameUnitFile toName file = subdir ++ toName ++ fileExtension file
   where subdir = concat . map (++ "/") . init . splitOn "/" $ file
