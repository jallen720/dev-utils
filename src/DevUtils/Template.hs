module DevUtils.Template (getTemplate) where


import System.Environment (getEnv)
import Data.String.Utils (replace)
import DevUtils.FileSystem (recursiveFileList)

import DevUtils.Utils
   ( ReplaceOp (..)
   , get
   , getReplaceOpChain )


getTemplatesDir :: IO String
getTemplatesDir =
   getEnv "PROJECTS" >>= return . (++ "/dev-utils/resources/templates/")


getTemplateFiles :: IO [String]
getTemplateFiles = getTemplatesDir >>= recursiveFileList []


getTemplateNames :: IO [String]
getTemplateNames =
   getTemplatesDir >>= \templatesDir ->
      getTemplateFiles >>= return . map (replace templatesDir "")


getTemplateSources :: IO [(String, String)]
getTemplateSources =
   getTemplateNames >>= \templateNames ->
      getTemplateFiles >>= mapM readFile >>= return . zip templateNames


printTemplateSources :: IO ()
printTemplateSources = getTemplateSources >>= mapM_ printTemplateSource
   where printTemplateSource (name, source) =
            putStrLn $ name ++ ":\n" ++ formatSource source

         formatSource = unlines . map ("    " ++) . lines


getTemplate :: String -> [ReplaceOp] -> IO String
getTemplate name replaceOps =
   getTemplateSources
      >>= return . getReplaceOpChain formattedReplaceOps . get name

   where formattedReplaceOps = getFormattedReplaceOps replaceOps


getFormattedReplaceOps :: [ReplaceOp] -> [ReplaceOp]
getFormattedReplaceOps = map formatReplaceOp
   where formatReplaceOp (ReplaceOp fromString toString) =
            ReplaceOp ("[[ " ++ fromString ++ " ]]") toString
