module DevUtils.Utils
   ( subString
   , lastIndex
   , get
   , keys
   , directify ) where


import Data.Maybe (fromJust)


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
