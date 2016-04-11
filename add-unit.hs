#!/usr/bin/runghc


import DevUtils.UI
   ( optionsPrompt
   , confirmationPrompt
   , nonEmptyInputPrompt
   , runUI
   , inputBlock )

import DevUtils.Unit
   ( UnitInput
   , Unit
   , unitFileData
   , createUnit
   , associatedFiles
   , createUnitFiles )

import DevUtils.Utils (get, keys)


main :: IO ()
main = runUI addUnitUI


addUnitUI :: IO ()
addUnitUI = promptUnitInput >>= confirmCreateUnitFiles . createUnit


promptUnitInput :: IO UnitInput
promptUnitInput = do
   unitInfo <- promptUnitInfo
   fileKeys <- promptFileKeys

   return
      ( get "unit name"         unitInfo
      , get "unit namespace"    unitInfo
      , get "unit subdirectory" unitInfo
      , fileKeys )


promptUnitInfo :: IO [(String, String)]
promptUnitInfo =
   inputBlock nonEmptyInputPrompt
      [ "unit name"
      , "unit namespace"
      , "unit subdirectory" ]


promptFileKeys :: IO [Char]
promptFileKeys = optionsPrompt "unit files" keyDescriptions keyOptions
   where keyDescriptions = map (showDescription . keyDescription) unitFileData
         keyDescription (key, keyData) = (key, get "description" keyData)
         showDescription (key, description) = key : ": " ++ description
         keyOptions = map (:"") (keys unitFileData)


confirmCreateUnitFiles :: Unit -> IO ()
confirmCreateUnitFiles unit = do
   confirmationPrompt "create unit files" (associatedFiles unit) (yes, no)
   where yes = createUnitFiles unit
         no = putStrLn "canceled"
