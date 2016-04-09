module DevUtils.UI (
   optionsPrompt,
   confirmationPrompt,
   nonEmptyInputPrompt,
   emptyLine)
where


import Data.Char (
   toUpper,
   toLower)

import Data.List (intercalate)

import System.IO (
   hFlush,
   stdout)


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
   getLine >>= return . optionOrDefault


buildOptionsMsg :: String -> [String] -> [String] -> String
buildOptionsMsg title msgLines options =
   buildMultiLineMsg title msgLines ++
   "    ? (" ++ showOptions ++ "): "
   where showOptions = intercalate "/" $ toUpperDefault options
         toUpperDefault (first:rest) = map toUpper first : rest


buildMultiLineMsg :: String -> [String] -> String
buildMultiLineMsg title msgLines =
   "\n" ++
   title ++ ":\n" ++
   showMsgLines
   where showMsgLines = unlines $ map ("    " ++) msgLines


confirmationPrompt :: String -> [String] -> (IO (), IO ()) -> IO ()
confirmationPrompt title msgLines (yesAction, noAction) =
   optionsPrompt title msgLines ["yes", "no"] >>= \answer -> do
      emptyLine
      if answer == "yes"
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
            putStr $ "\n" ++
                     "    " ++ show title ++ " can't be empty!\n" ++
                     "\n"

            nonEmptyInputPrompt title


prompt :: String -> IO ()
prompt msg = do
   putStr msg
   hFlush stdout


emptyLine :: IO ()
emptyLine = putStr "\n"
