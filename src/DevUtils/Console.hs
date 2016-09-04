module DevUtils.Console (validateArgs) where


import Data.List (intercalate)


validateArgs :: [String] -> [String] -> IO ()
validateArgs args usageArgNames =
   if length args == length usageArgNames
      then return ()
      else error usageMessage

   where usageMessage = "usage: " ++ intercalate " " usageArgNames
