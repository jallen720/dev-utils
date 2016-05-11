module DevUtils.Utils
   ( ReplaceOp (..)
   , subString
   , lastIndex
   , get
   , keys
   , directify
   , extensionify
   , quote
   , getReplaceOpChain ) where


import Data.Maybe (fromJust)
import Data.String.Utils (replace)


data ReplaceOp =
   ReplaceOp
      { fromString :: String
      , toString   :: String }


subString :: Int -> String -> String
subString 0 _ = ""
subString _ "" = ""
subString length (char:rest) = char : subString (length - 1) rest


lastIndex :: (Eq a) => a -> [a] -> Int
lastIndex _ [] = 0
lastIndex item (currentItem:rest) =
   if shouldAddIndex
      then 1 + restIndex
      else 0

   where shouldAddIndex = item == currentItem || restIndex > 0
         restIndex = lastIndex item rest


get :: (Eq a) => a -> ([(a, b)] -> b)
get key = fromJust . lookup key


keys :: [(a, b)] -> [a]
keys [] = []
keys ((key, _):rest) = key : keys rest


directify :: String -> String
directify dir =
   if isValidDir
      then dir
      else dir ++ "/"

   where isValidDir = last dir == '/'


extensionify :: String -> String
extensionify extension =
   if isValidExtension
      then extension
      else '.' : extension

   where isValidExtension = head extension == '.'


quote :: String -> String
quote = ("\"" ++) . (++ "\"")


getReplaceOpChain :: [ReplaceOp] -> (String -> String)
getReplaceOpChain = foldl linkReplaceOp id
   where linkReplaceOp replaceOpChain (ReplaceOp fromString toString) =
            replaceOpChain . replace fromString toString
