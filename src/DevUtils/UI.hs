module DevUtils.UI
   ( optionsPrompt
   , confirmationPrompt
   , nonEmptyInputPrompt
   , runUI
   , inputBlock ) where


import System.IO (hFlush, stdout)
import Data.Char (toUpper, toLower)
import Data.List (intercalate)


optionsPrompt :: String -> [String] -> [String] -> IO String
optionsPrompt _ [] _ = error "options prompt must have message lines"
optionsPrompt _ _ [] = error "options prompt must have options"
optionsPrompt title msgLines options = do
   let
      optionOrDefault option =
         if option == ""
            then options !! 0
            else map toLower option

   prompt $ buildOptionsMsg title msgLines options
   response <- getLine
   emptyLine
   return . optionOrDefault $ response


buildOptionsMsg :: String -> [String] -> [String] -> String
buildOptionsMsg title msgLines options =
   buildMultiLineMsg title msgLines ++
   "    ? (" ++ showOptions ++ "): "

   where showOptions = intercalate "/" $ toUpperDefault options
         toUpperDefault (first:rest) = map toUpper first : rest


buildMultiLineMsg :: String -> [String] -> String
buildMultiLineMsg title msgLines =
   title ++ ":\n" ++
   showMsgLines msgLines
   where showMsgLines = unlines . map ("    " ++)


confirmationPrompt :: String -> [String] -> (IO (), IO ()) -> IO ()
confirmationPrompt title msgLines (yesAction, noAction) =
   optionsPrompt title msgLines ["yes", "no"] >>= selectAction
   where selectAction response =
            if response == "yes"
               then yesAction
               else noAction


inputPrompt :: String -> IO String
inputPrompt title = do
   prompt $ title ++ ": "
   getLine


nonEmptyInputPrompt :: String -> IO String
nonEmptyInputPrompt title = inputPrompt title >>= checkInput
   where checkInput input =
            if input == ""
               then retry
               else return input

         retry = do
            putStr . unlines $
               [ ""
               , "    " ++ show title ++ " can't be empty!"
               , "" ]

            nonEmptyInputPrompt title


prompt :: String -> IO ()
prompt msg = do
   putStr msg
   hFlush stdout


runUI :: IO () -> IO ()
runUI ui = do
   emptyLine
   ui
   emptyLine


emptyLine :: IO ()
emptyLine = putStr "\n"


inputBlock :: (String -> IO String) -> [String] -> IO [(String, String)]
inputBlock prompter promptTitles = do
   let
      prompt promptTitle = do
         response <- prompter promptTitle
         return (promptTitle, response)

   responses <- mapM prompt promptTitles
   emptyLine
   return responses
