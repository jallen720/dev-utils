#!/usr/bin/runghc

import DevUtils.UI (
   optionsPrompt,
   confirmationPrompt,
   nonEmptyInputPrompt,
   emptyLine)

import DevUtils.Unit (
   UnitInput,
   Unit,
   unitFileData,
   createUnit,
   associatedFiles,
   createUnitFiles)

import DevUtils.Utils (
   get,
   keys)


main :: IO ()
main = promptUnitInput >>= confirmCreateUnitFiles . createUnit


promptUnitInput :: IO UnitInput
promptUnitInput = do
   emptyLine
   name      <- nonEmptyInputPrompt "unit name"
   namespace <- nonEmptyInputPrompt "unit namespace"
   subDir    <- nonEmptyInputPrompt "unit subdirectory"
   fileKeys  <- promptFileKeys
   return (name, namespace, subDir, fileKeys)


promptFileKeys :: IO [Char]
promptFileKeys = optionsPrompt "unit files" keyDescriptions keyOptions
   where keyDescriptions = map (showDescription . keyDescription) unitFileData
         keyDescription (key, keyData) = (key, get "description" keyData)
         showDescription (key, description) = key : ": " ++ description
         keyOptions = map (:"") (keys unitFileData)


confirmCreateUnitFiles :: Unit -> IO ()
confirmCreateUnitFiles unit = do
   confirmationPrompt "create" (associatedFiles unit) (yes, no)
   emptyLine
   where yes = createUnitFiles unit
         no = putStrLn "canceled"
