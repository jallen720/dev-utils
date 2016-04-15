import Data.List (nub)

import DevUtils.UI
   ( optionsPrompt
   , confirmationPrompt
   , nonEmptyInputPrompt
   , runUI
   , inputBlock )

import DevUtils.Unit
   ( UnitInput
   , FileKey
   , Unit
   , unitFileData
   , createUnit
   , associatedFiles
   , createUnitFiles )

import DevUtils.Utils (get, keys)


main :: IO ()
main = runUI addUnitUI


addUnitUI :: IO ()
addUnitUI = do
   unitInput <- promptUnitInput
   fileKeys <- promptFileKeys
   confirmCreateUnitFiles (createUnit unitInput) fileKeys


promptUnitInput :: IO UnitInput
promptUnitInput =
   inputBlock nonEmptyInputPrompt
      [ "unit name"
      , "unit namespace"
      , "unit subdirectory" ]
   >>= return . packageUnitInput

   where packageUnitInput unitInput =
            ( get "unit name"         unitInput
            , get "unit namespace"    unitInput
            , get "unit subdirectory" unitInput )


promptFileKeys :: IO [FileKey]
promptFileKeys =
   optionsPrompt "unit files" keyDescriptions keyOptions >>= return . uniqueKeys
   where keyDescriptions = map (showDescription . keyDescription) unitFileData
         keyDescription (key, keyData) = (key, get "description" keyData)
         showDescription (key, description) = key : ": " ++ description
         keyOptions = map (:"") (keys unitFileData)
         uniqueKeys fileKeys = nub . filter isFileKey $ fileKeys
         isFileKey = (`elem` keys unitFileData)


confirmCreateUnitFiles :: Unit -> [FileKey] -> IO ()
confirmCreateUnitFiles unit fileKeys =
   confirmationPrompt "create unit files" unitFiles (yes, no)
   where unitFiles = associatedFiles unit fileKeys
         yes = createUnitFiles unit fileKeys
         no = putStrLn "canceled"
