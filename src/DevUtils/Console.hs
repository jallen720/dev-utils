module DevUtils.Console (validateArgs) where


import Data.List (intercalate)


validateArgs :: [String] -> Int -> [String] -> IO ()
validateArgs args validArgCount usageArgNames =
   if length args == validArgCount
      then return ()
      else error usageMessage

   where usageMessage = "usage: " ++ intercalate " " usageArgNames
